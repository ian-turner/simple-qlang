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

All required pipeline stages are now landed. The current working examples
(`bell00`, `tele`, `ghz`) all compile to correct OpenQASM.

Remaining refinement work (none blocking):

- reusable `gate` / `def` declaration emission (currently inlines from `output`)
- explicit loop IR and static-list erasure for bounded recursion (currently
  budget-unrolling in the emitter)
- gate/`def` classification for all closed helpers, not just top-level
  interface-flattened declarations
