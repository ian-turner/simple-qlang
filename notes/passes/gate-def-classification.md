# Gate/Def Classification

**Module:** `src/GateDef.hs`

See also: [../pipeline.md](../pipeline.md)

---

## Overview

OpenQASM distinguishes two kinds of subroutine:
- **`gate`**: pure unitary — body contains only gate applications
- **`def`**: subroutine with measurement or classical control

The classification pass analyzes the interface-flattened CPS for each top-level
declaration and assigns it `Gate` or `Def`. Today this information is carried in
`CompiledModule` and shown in `--debug` output; the first-cut emitter does not
yet consume it to emit reusable OpenQASM `gate`/`def` declarations.

---

## Classification heuristic

The pass is intentionally conservative and targets top-level declarations only.

**`gate`**: the interface-flattened body contains only direct quantum gate
primitives (`hgate`, `xgate`, `zgate`, `cnot`, etc.) plus continuation
plumbing.

**`def`**: the body contains any of:
- `init`
- `meas`
- classical `switch`
- classical arithmetic / comparison / logic
- a call to another top-level declaration already classified as `def`

Calls through continuation variables remain neutral — ordinary CPS sequencing
does not force every function to become a `def`.

---

## Why it runs on interface-flattened CPS

The classifier runs after interface flattening but **before** closure
conversion. This placement:
- Gives stable `gate`/`def` summaries for top-level declarations
- Avoids closure-conversion or dispatch scaffolding dominating the result
  (after closure conversion, almost everything looks like it has extra records
  and indirect calls)

---

## Current limitation

This is not yet the final backend-facing classification:
- Does not classify every closed internal helper produced by later stages
- Calls through higher-order parameters are treated conservatively-neutral
  rather than being classified from actual call sites
- The first-cut emitter still inlines from `output`, so callable kinds are
  currently analysis/debug metadata rather than a direct emission control path

That tradeoff is deliberate: it gives a stable notion of top-level `gate` vs
`def` without requiring full call-site analysis.
