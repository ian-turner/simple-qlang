# Record Flattening Stage

Implemented a first conservative record-flattening pass in `src/RecordFlatten.hs`.

## Current Strategy

- The pass runs after qubit hoisting
- It tracks record layouts that are statically known within the current CPS
  expression
- `CSelect` and `COffset` are folded away when their source record can be
  resolved from that tracked layout
- `CRecord` bindings are dropped when all downstream uses are eliminated by
  that substitution

## What This Covers

- administrative tuple records introduced by CPS conversion when they stay
  within one declaration
- locally constructed closure records whose fields are immediately projected
  with `SELECT`/`OFFSET`
- records containing hoisted qubits, which are preserved as `VQubit` leaves

## Current Limitation

This is not yet the full required tuple/record-flattening stage described in
`notes/cps-compilation-strategy.md`.

The remaining gap is interprocedural shape propagation:

- if a record value escapes through a function parameter or return path, the
  current pass leaves that record in place
- top-level declarations are still compiled independently in `src/Main.hs`, so
  cross-declaration record arguments/results are not rewritten together
- later work needs a module-level pass that rewrites function signatures and
  call sites consistently once record shapes are known

## Why This Version Exists

The current pass is still useful because it removes the easy, local record
traffic first and makes the next stage clearer:

- it reduces noise in the post-hoisting CPS
- it exercises record-shape tracking before OpenQASM emission exists
- it leaves escaping records explicit, which makes the missing interprocedural
  work easy to spot in example output
