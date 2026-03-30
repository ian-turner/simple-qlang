# Bounded Recursion Lowering Plan

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
