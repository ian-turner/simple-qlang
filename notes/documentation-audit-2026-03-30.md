# Documentation Audit 2026-03-30

This note records a repo-wide markdown consistency pass across the top-level
docs, agent guidance, and implementation notes.

## Synced Topics

- README language status now says linear typing is planned but still deferred.
- README recursion status now matches the implementation:
  local recursive `fix` groups are rejected, while a bounded top-level
  self-recursive subset is supported by backend unrolling.
- README built-in primitive list now includes the currently implemented
  quantum gates beyond `hgate` / `xgate` / `zgate` / `cnot`.
- README and status docs now mention current OpenQASM backend behavior:
  homogeneous output arrays / bit vectors and two-arm boolean `if/else`
  rendering.
- `CLAUDE.md` no longer says bounded-recursion unrolling is deferred.
- `notes/current-pipeline-status.md` no longer refers to stale CLI debug-output
  behavior or per-leaf-only output declarations.
- `notes/module-compile-pipeline.md` no longer says backend emission is still
  missing.
- `notes/float-constants-2026-03-30.md` now matches the current output-layout
  policy.

## Remaining Intentional Gaps

- There is still no automated test suite.
- Linear type checking is still deferred.
- Reusable OpenQASM `gate` / `def` emission is still future work.
- Post-branch continuation sharing is still not implemented; `if/else`
  rendering is currently syntax-only.
