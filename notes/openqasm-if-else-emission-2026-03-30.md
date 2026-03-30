# OpenQASM If/Else Emission

This note records a rendering-only backend change.

## What Changed

- `src/OpenQASM.hs` now renders dynamic two-arm `CSwitch` branches as OpenQASM
  `if/else` statements instead of `switch`.
- The emitted condition is `scrutinee == 1`, so arm 1 becomes the `if` body
  and arm 0 becomes the `else` body.
- Branches with any other arity still emit as `switch`.

## Scope

This does not change CPS lowering or backend control-flow structure.

The emitter still lowers each branch independently through the rest of the
continuation, so downstream code duplication remains. This change only improves
the generated OpenQASM surface syntax for boolean-style branching.

## Validation

- `cabal build`
- run the built compiler on `examples/tele.funq` and confirm nested
  `if/else` output
