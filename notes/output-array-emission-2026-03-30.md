# Output Array Emission

This note records a backend-only change to OpenQASM emission.

## What Changed

- `src/OpenQASM.hs` now chooses an output layout after flattening the final
  classical result leaves.
- If every flattened output leaf has the same classical type and there is more
  than one leaf, the emitter uses a single indexed output container instead of
  separate `output_i` variables.
- `bit` outputs use `bit[n] output;`.
- Other homogeneous classical outputs use `array[type, n] output;`.
- Mixed-type outputs still emit separate `output_i` declarations because
  OpenQASM arrays are homogeneous.

## Why This Is Backend-Only

The middle end still flattens tuple/data-flow records into independent scalar
leaves. That remains the right internal representation because the backend
still needs direct scalar values for primitive emission and mixed-type tuples
cannot become a single OpenQASM array.

This change only affects how the final flattened leaves are named in emitted
OpenQASM.

## Validation

- `cabal build`
- `cabal run funq -- examples/bell00.funq`
- `cabal run funq -- examples/qft.funq`
- `cabal run funq -- examples/ghz.funq`
- `cabal run funq -- examples/float_constants.funq`
- `cabal run funq -- examples/countdown.funq`
