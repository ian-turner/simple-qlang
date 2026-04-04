# Bounded Recursion Lowering — Future Plan

See also: [../passes/recursion.md](../passes/recursion.md), [../pipeline.md](../pipeline.md)

---

## Current state

Self-recursive tail loops compile to OpenQASM `while` loops via `isTailLoop`.
All other recursive programs — including structural list recursion —  are
handled by budget-unrolling in the emitter (depth limit: 1000 calls). This
covers current examples like `ghz.funq` and `qft_n.funq` but is fragile because
recursive ADT semantics are split across CPS lowering, interface flattening, and
emitter-side `ValueRep` reconstruction.

---

## Goal

Support recursive functions that can be rewritten into finite loops or
fixed-size dataflow before backend emission, without attempting general
recursion.

Target patterns:
- Counted self-recursion over `Int`
- Structural recursion over statically sized lists
- Explicit loop and fixed-size aggregate lowering before closure conversion

Anything outside that fragment should continue to produce a compile-time error.

---

## Proposed new passes

### 1. Static shape inference (`src/StaticShape.hs`)

Infer finite list lengths and aggregate shapes from top-level bindings:

```haskell
data Size
  = SizeKnown Int
  | SizeVar Variable
  | SizeUnknown

data Shape
  = ShapeScalar | ShapeQubit | ShapeBool
  | ShapeTuple [Shape]
  | ShapeList Size Shape
  | ShapeUnknown
```

Examples:
- `init_n n` produces `List Qubit` of length `n`
- `meas_all xs` preserves input list length
- `ghz_4` has concrete length `4`

Conservative: anything not proven bounded remains `SizeUnknown` and must be
rejected if it participates in recursion.

### 2. Bounded recursion lowering

Replace reject-only recursion handling with a pass that:
- Recognizes bounded recursive patterns
- Rewrites them into explicit loop IR
- Rejects everything that remains unsupported

Proposed CPS extension (`src/CPSExp.hs`):

```haskell
| CFor
    Variable      -- index variable
    Value         -- trip count
    [Variable]    -- loop-carried variables
    [Value]       -- initial carried values
    CExp          -- body
    CExp          -- exit continuation
```

List recursion should be lowered into counted iteration rather than a separate
list-specific loop node if possible.

### 3. Static list erasure (`src/StaticListErase.hs`)

After bounded list recursion is lowered:
- Erase `List a` values with known finite size into fixed records/tuples
- Remove `Nil`/`Cons` from the backend-facing IR
- Let existing record flattening handle the resulting aggregates

This is required because OpenQASM has no runtime algebraic data structures.

---

## Pipeline placement

```
1. Parse / scope / Lambda lowering
2. CPS conversion
3. Static shape inference        ← new
4. Bounded recursion lowering    ← new
5. Residual recursion rejection
6. Static list erasure           ← new
7. Interface flattening
8. Gate/def classification
9. Closure conversion
10. Defunctionalization
11. Qubit hoisting
12. Local record flattening
13. OpenQASM emission
```

---

## GHZ mapping

`examples/ghz.funq` needs all three planned pieces:
- `init_n`: counted recursion constructing a fixed-size list of qubits
- `cnot_layer`: structural recursion over a statically sized list with
  loop-carried qubit state
- `meas_all`: structural recursion over a statically sized list producing a
  fixed-size list of bits

Currently `ghz.funq` compiles via budget-unrolling (depth 1000). The three-pass
plan above is the right long-term replacement.

---

## Recommended implementation order

1. Add loop IR to `src/CPSExp.hs`
2. Implement counted-`Int` recursion lowering first
3. Emit counted loops in `src/OpenQASM.hs` (OpenQASM 3.0 `for` or `while`)
4. Add static list-length inference
5. Add structural list recursion lowering
6. Add static list erasure to records/tuples
7. Re-run `examples/ghz.funq` and adjust later passes

---

## Why the current approach is transitional

Budget-unrolling works for existing examples but is fragile because recursive
ADT semantics are currently split across three places:
- CPS lowering conventions (how `Nil`/`Cons` become tagged records)
- Interface flattening (how those tagged records flow across boundaries)
- Emitter-side `ValueRep` reconstruction (how the emitter reconstructs ADT
  shape during recursive evaluation)

The Phase 1–3 plan (shape inference → bounded recursion IR lowering → static
list erasure) moves this logic into the middle end where it belongs, and
simplifies the emitter to a pure OpenQASM renderer.
