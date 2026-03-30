# Record Flattening Next Step

This note is a historical handoff. The step described here has since been
completed; keep it only as context for how the continuation-result follow-on
work was framed at the time.

## What Is Already Landed

- local record simplification in `src/RecordFlatten.hs`
- module-level compilation in `src/CompilePipeline.hs`
- whole-module interface-shape inference in `src/RecordShape.hs`
- first interface rewrite pass in `src/ModuleRecordFlatten.hs`

## Completion Status

The work described below is now landed and documented more fully in
`notes/continuation-result-record-flattening.md`.

Tuple/data-flow results now flatten across top-level continuation boundaries
when shape analysis can connect the producer and consumer interfaces.

The current next required stage is OpenQASM emission, with gate/`def`
classification already landed as a conservative top-level pass in
`src/GateDef.hs`.

## Conservative Rule

Prefer not flattening to flattening the wrong thing.

That rule still applies in the current implementation: interfaces whose shape
is unknown, opaque, closure-like, or defunctionalization-specific remain
conservative.
