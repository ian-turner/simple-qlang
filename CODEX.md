# CODEX.md

Codex-specific project context for FunQ.

## What Matters Most

- Preserve a clean separation between backend-neutral IR passes and
  OpenQASM-specific lowering
- Do not introduce backend assumptions early unless they are recorded as a
  deliberate design decision
- When a design choice changes the pipeline or semantics, update the matching
  notes in `notes/`

## Pipeline Summary

1. Parse and resolve names
2. Lower to Lambda IR
3. Convert to CPS
4. Check/eliminate recursion (reject recursive local `CFix`; allow bounded top-level self-recursion via `src/BoundedRecursion.hs`)
5. Module-level record-shape inference (`src/RecordShape.hs`)
6. Interface record flattening (`src/ModuleRecordFlatten.hs`)
7. Classify `gate` vs `def` (`src/GateDef.hs`)
8. Closure convert
9. Defunctionalize
10. Qubit hoisting
11. Local record flattening (`src/RecordFlatten.hs`)
12. Emit OpenQASM

## Semantics To Preserve

- Qubits are linear values and must not be duplicated
- Measurement consumes qubits and yields classical control
- Closure conversion must not be used as an excuse to hide qubit-lifetime
  issues; later typing rules should still reject invalid post-measurement use

## Practical Workflow

- Read `README.md`, `AGENTS.md`, and the relevant note in `notes/` before
  changing pipeline code
- Prefer checking `src/Main.hs`, `src/CPSExp.hs`, `src/RecElim.hs`, and
  `src/ClosureConv.hs` first when working on compiler stages
- Verify meaningful compiler changes with `cabal build` and at least one
  example run

## Current Gaps

- No type checker yet
- No automated test suite yet
- OpenQASM emitter exists but does not yet emit reusable `gate` / `def` declarations (inlines everything from `output`)
- Bounded recursion is supported via budget-unrolling; explicit loop IR and static-list erasure are future work
- QIR remains future scope only; keep current code focused on OpenQASM while
  preserving backend-neutral middle-end structure
