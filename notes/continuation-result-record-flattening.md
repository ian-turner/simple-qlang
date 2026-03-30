# Continuation-Result Record Flattening

This note records the change that completes the current tuple/data-flow
record-flattening milestone.

## What Changed

- `src/RecordShape.hs` now tracks callable parameter slots with a dedicated
  `ParamFunction` key.
- Shape analysis now records arities for calls made through continuation
  parameters, not just `CFix`-bound functions and top-level labels.
- When a callable argument is passed into a parameter slot whose call arity is
  already known, the callee-side continuation interface shape is propagated
  back to the actual function supplied at the call site.
- `src/ModuleRecordFlatten.hs` now binds parameter-call keys while rewriting a
  function body, so calls through continuation parameters can use those
  inferred shapes during interface rewriting.

## Effect

This lets tuple/data-flow results flatten across top-level continuation
boundaries.

The concrete symptom that is now removed is the tuple handoff pattern:

`CRecord [(v0, OFFp 0), (v1, OFFp 0)] tmp (CApp k [tmp])`

when `k` is a continuation parameter supplied by another declaration. After
the change, that handoff rewrites to a direct scalar continuation call:

`CApp k [v0, v1]`

and the caller-side continuation definition is rewritten to accept flattened
parameters.

## Verification

Validated with:

- `cabal build`
- `cabal run funq -- examples/bell00.funq`
- `cabal run funq -- examples/tele.funq`

Observed outcomes:

- `bell00`'s tuple result path into `output` now flattens across the
  declaration boundary.
- `tele`'s tuple-valued data-flow through `bell00` and local continuation
  chains also rewrites to flattened continuation interfaces.

## Remaining Boundary

This completes the current required tuple/data-flow flattening stage, but it
does not flatten closure-conversion or defunctionalization records
aggressively. Those remain conservative by design and should be handled, if
needed, by later backend-facing classification/lowering work.

## Next Stage

The next required implementation step is gate/`def` classification over the
closed, flattened representation, followed by OpenQASM emission.
