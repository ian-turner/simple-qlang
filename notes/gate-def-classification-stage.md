# Gate/Def Classification Stage

This note records the first landed gate/`def` classification pass.

## What Changed

- `src/GateDef.hs` adds a module-level classification analysis for top-level
  callables.
- `src/CompilePipeline.hs` now computes interface-flattened CPS first, runs the
  classifier over that module view, then continues with closure conversion,
  defunctionalization, qubit hoisting, and local record flattening.
- `src/Main.hs` prints both per-declaration classifications and a module
  summary.

## Current Heuristic

The classifier is intentionally conservative and currently targets top-level
 declarations only.

- `gate` means the interface-flattened body contains only direct quantum gate
  primitives (`hgate`, `xgate`, `zgate`, `cnot`) plus continuation plumbing.
- `def` means the body contains any of:
  - `init`
  - `meas`
  - classical `switch`
  - classical arithmetic / comparison / logic
  - a call to another top-level declaration already classified as `def`

Calls through continuation variables remain neutral so ordinary CPS sequencing
does not force every function to become a `def`.

## Important Limitation

This is not yet the final backend-facing classification.

- The current pass does not classify every closed internal helper produced by
  later stages.
- Calls through higher-order parameters are treated conservatively-neutral
  rather than being classified from actual call sites.
- The analysis runs on interface-flattened CPS rather than the final
  defunctionalized form so it can still distinguish ordinary gate flow from
  backend-introduced closure/dispatch scaffolding.

That tradeoff is deliberate for now: it gives the pipeline a stable notion of
top-level `gate` vs `def` without letting closure-conversion artifacts collapse
everything into `def`.
