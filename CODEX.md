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
4. Check/eliminate recursion before closure conversion
5. Closure convert
6. Defunctionalize
7. Perform backend-specific qubit hoisting
8. Flatten tuples/records
9. Classify `gate` vs `def`
10. Emit OpenQASM

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
- No backend emitter yet
- Several notes still mention QIR as future scope; keep current code focused on
  OpenQASM while preserving backend-neutral middle-end structure
