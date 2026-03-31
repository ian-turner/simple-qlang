# Bounded Recursion Lowering Plan

> **Status**: Future design proposal. This document describes a planned loop-IR
> approach that is not yet implemented. For the current implementation (counter-
> based unrolling in `src/BoundedRecursion.hs`), see
> `notes/bounded-recursion-first-pass-2026-03-30.md`.

This note captures the current design direction for supporting finite recursive
programs in the OpenQASM pipeline without attempting general recursion.

## Goal

Support recursive functions that can be rewritten into finite loops or finite
fixed-size dataflow before backend emission.

The motivating example is `examples/ghz.funq`, whose helpers are logically
terminating but are currently rejected because the compiler only has
reject-or-inline behavior for recursion.

## Scope

The target is not "support arbitrary recursion". The target is:

- counted self-recursion over `Int`
- structural recursion over statically sized lists
- explicit loop and fixed-size aggregate lowering before closure conversion

Anything outside that fragment should continue to produce a compile-time
error.

## Proposed New Passes

### 1. Static shape inference

New pass, likely `src/StaticShape.hs`.

Purpose:

- infer finite list lengths and aggregate shapes
- track known sizes through top-level bindings and local CPS values
- provide boundedness facts to recursion lowering

Expected facts:

- `init_n n` returns a list of length `n`
- `meas_all xs` preserves the length of `xs`
- `ghz_4` has concrete length `4`

Suggested abstract domain:

```haskell
data Size
  = SizeKnown Int
  | SizeVar Variable
  | SizeUnknown

data Shape
  = ShapeScalar
  | ShapeQubit
  | ShapeBool
  | ShapeTuple [Shape]
  | ShapeList Size Shape
  | ShapeUnknown
```

### 2. Bounded recursion lowering

This should replace the current reject-only behavior in `src/RecElim.hs`.

Purpose:

- recognize bounded recursive patterns
- rewrite them into explicit loop IR
- reject everything that remains unsupported

Likely accepted patterns for v1:

- counted self-tail recursion over `Int`
- structural recursion over statically sized lists
- no mutual recursion
- no measurement-dependent recursion

Suggested CPS extension in `src/CPSExp.hs`:

```haskell
| CFor
    Variable      -- index variable
    Value         -- trip count
    [Variable]    -- loop-carried variables
    [Value]       -- initial carried values
    CExp          -- body
    CExp          -- exit continuation
```

The preference is to lower list recursion into counted iteration rather than
introducing a second list-specific loop node unless the implementation clearly
needs one.

### 3. Static list erasure

New pass, likely `src/StaticListErase.hs`.

Purpose:

- erase statically sized `List` values into fixed records/tuples
- remove `Nil`/`Cons` from the backend-facing part of the pipeline
- let existing record flattening handle the resulting aggregates

This is required because OpenQASM has no runtime algebraic data structures.

## Pipeline Placement

Proposed pipeline:

1. Parse / scope / Lambda lowering
2. CPS conversion
3. Static shape inference
4. Bounded recursion lowering
5. Residual recursion rejection
6. Static list erasure
7. Interface flattening
8. Callable classification
9. Closure conversion
10. Defunctionalization
11. Qubit hoisting
12. Record flattening
13. OpenQASM emission

The important point is that recursion and list structure stay visible until the
compiler has had a chance to prove boundedness and lower them.

## GHZ Mapping

`examples/ghz.funq` needs all three planned pieces:

- `init_n`: counted recursion that constructs a fixed-size list of qubits
- `cnot_layer`: structural recursion over a statically sized list with
  loop-carried qubit state
- `meas_all`: structural recursion over a statically sized list producing a
  fixed-size list of bits

Counted loops alone are not enough to support GHZ as written. GHZ needs both:

- bounded recursion lowering
- static list erasure

## Existing Stages That Must Change

### `src/RecElim.hs`

Current state:

- checks `CFix` groups for recursion
- rejects any recursive local function

Planned state:

- try bounded-recursion lowering first
- reject only the recursive forms that survive lowering

### `src/CompilePipeline.hs`

Current state:

- runs reject-only local recursion checking
- rejects top-level recursive `VLabel` cycles before backend emission

Planned state:

- run shape inference before recursion lowering
- run top-level cycle rejection after bounded lowering
- reject only residual unsupported cycles

### `src/OpenQASM.hs`

Current state:

- emits straight-line statements and switches

Planned state:

- emit counted loop IR as OpenQASM `for`
- rely on earlier static-list erasure so the emitter does not need direct list
  support

## Recommended Implementation Order

1. Add loop IR to `src/CPSExp.hs`.
2. Implement counted-`Int` recursion lowering first.
3. Emit counted loops in `src/OpenQASM.hs`.
4. Add static list-length inference.
5. Add structural list recursion lowering.
6. Add static list erasure to records/tuples.
7. Re-run `examples/ghz.funq` and adjust later passes where necessary.

## Practical First Cut

The safest first milestone is not full GHZ support. It is:

- integer-counted bounded recursion
- OpenQASM `for` emission
- continued rejection of recursive list programs

That gets the loop representation and backend path correct before taking on the
harder static-list lowering work.

## Robustness Plan Beyond The First Cut

The first bounded-recursion slice is now landed, but the current backend is
still recovering some recursive data structure semantics inside
`src/OpenQASM.hs`. That is enough to get closer to examples like GHZ, but it is
not the right long-term boundary for recursive programs in general.

The robust direction is:

1. finish the current GHZ blocker only as needed to keep momentum
2. move bounded recursion handling out of the emitter and into explicit IR
   lowering
3. erase statically sized lists before the backend instead of reconstructing
   `Nil` / `Cons` behavior from tagged records during emission

That should make recursive programs much more stable and reduce backend-only
special cases.

### Why the current approach is fragile

The current backend now has:

- top-level recursion budgeting
- constructor-aware switching on tagged values
- partial constructor-aware output flattening
- partial recovery of list payload shapes while executing recursive programs

This is useful as a bridge, but it means recursive ADT semantics are currently
split across:

- CPS lowering conventions
- interface flattening
- emitter-side `ValueRep` reconstruction

That split is exactly what makes examples like `ghz.funq` brittle: one path can
still collapse a `Cons` value to a bare constructor tag and then later code
tries to deconstruct it as if the payload were still present.

### Recommended robust implementation sequence

#### Phase 1. Finish the current GHZ blocker narrowly

Purpose:

- keep the repository in a usable state
- confirm the remaining issue is only an emitter-side ADT representation bug

This phase should stay small and should not become the long-term architecture.

#### Phase 2. Add bounded shape/size inference

Purpose:

- infer fixed-size list lengths and bounded aggregate shape
- provide a reliable proof that recursive list traversals are finite

This should become the source of truth for:

- `init_n n` has length `n`
- `meas_all xs` preserves input length
- `ghz_4` has concrete length `4`

#### Phase 3. Add explicit bounded-recursion IR lowering

Purpose:

- lower supported recursive programs into explicit finite iteration
- stop relying on recursive evaluation in the backend

This is where counted recursion and structural list recursion should become:

- counted loops
- loop-carried state
- indexed access into fixed aggregates

#### Phase 4. Add static-list erasure

Purpose:

- erase `List` values with known finite size into fixed aggregates
- remove `Nil` / `Cons` from the backend-facing IR

After this phase, the backend should not need to reconstruct ADTs from tagged
records. It should only see:

- classical scalars
- qubit values
- fixed tuples / records
- explicit loops and switches

#### Phase 5. Simplify the emitter

Purpose:

- remove constructor/list recovery logic from `src/OpenQASM.hs`
- keep the backend focused on OpenQASM rendering rather than semantic recovery

At that point the backend becomes much easier to reason about, and recursive
program support becomes a middle-end feature rather than a backend side effect.

## Current GHZ-specific status

As of 2026-03-30, `examples/ghz.funq` compiles and produces correct OpenQASM.

The previous emitter-side ADT consistency bug (`select 0 from VVar a saw scalar
1`) was traced to a type error in the source program, not a compiler limitation:

- `cnot_layer`'s Cons arm was returning a `List Qubit` value instead of the
  declared `(Qubit, List Qubit)` pair
- the recursive call was also threading `x'` (updated target) instead of `a'`
  (updated control) as the carried qubit

Both were fixed directly in `examples/ghz.funq`. The backend's recursive
evaluation via the 10,000-call budget counter handles `init_n`, `cnot_layer`,
and `meas_all` correctly once the input is well-typed.

The generated circuit is correct: `h q[0]`, then `cx q[0], q[1..3]`, then
measure all four qubits.

The larger lesson — that ADT semantics are split across CPS lowering,
interface flattening, and emitter-side `ValueRep` reconstruction — still
holds. The Phase 2–5 plan (shape inference → bounded recursion IR lowering →
static list erasure → emitter simplification) remains the right long-term
direction but is not blocking current examples.
