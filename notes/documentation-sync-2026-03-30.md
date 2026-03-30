# Documentation Sync 2026-03-30

This note records a documentation refresh after the landing of:

- continuation-result tuple/data-flow record flattening
- the first gate/`def` classification pass

## Updated Documents

- `AGENTS.md`
- `CLAUDE.md`
- `notes/current-pipeline-status.md`
- `notes/module-compile-pipeline.md`
- `notes/module-level-record-flattening-plan.md`
- `notes/module-record-shape-analysis.md`
- `notes/record-flattening-next-step.md`
- `notes/cps-compilation-strategy.md`

## Current Source Of Truth

For current implementation status, prefer:

- `notes/current-pipeline-status.md`
- `notes/continuation-result-record-flattening.md`
- `notes/gate-def-classification-stage.md`

## Current Remaining Required Stage

The remaining required backend milestone is OpenQASM emission.

Gate/`def` classification is landed, but it is currently a conservative
top-level analysis over interface-flattened CPS rather than the final
backend-facing classification for every closed helper.
