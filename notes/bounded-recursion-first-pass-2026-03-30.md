# Bounded recursion first pass

This repository now has a first implementation step toward recursion support.

Current supported subset:

- top-level self-recursive declarations only
- exactly one source argument, represented in CPS as params `[counter, k]`
- recursion guarded by `counter == 0`
- recursive branch must make one self-call on `counter - 1`
- the counter must be a compile-time constant when the declaration is called

Important limitations:

- no mutual recursion
- no list recursion yet
- no dynamic loop bounds yet
- unsupported recursive declarations are still rejected before backend emission

Implementation notes:

- `src/BoundedRecursion.hs` recognizes the accepted pre-closure CPS shape
- `src/CompilePipeline.hs` now allows that narrow self-recursive subset to pass
  the top-level recursion check
- `src/OpenQASM.hs` caches those declarations as special callables and expands
  them with an explicit recursion budget so emitter bugs cannot run forever

This is intentionally an unrolling-style first cut, not the future loop-IR
design described in `notes/bounded-recursion-lowering-plan-2026-03-30.md`.
