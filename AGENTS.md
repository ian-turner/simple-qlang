# AGENTS.md

Shared instructions for coding agents working in this repository.

## Working Rules

- When you make changes, place any notes that will be useful for future
  development in the `notes/` folder as markdown files
- Prefer small, targeted edits over broad refactors
- Keep the compilation pipeline documentation aligned with implementation when
  stage ordering or semantics change

## Current Compiler State

- Implemented: parse, scope resolution, lowering to Lambda IR, CPS conversion,
  recursion checking, closure conversion, defunctionalization, qubit hoisting,
  tuple/data-flow record flattening, gate/def classification, first OpenQASM
  emission
- Not yet implemented: reusable `gate` / `def` emission and backend cleanup
- There is no automated test suite yet; validate changes by building and
  running the example programs

## Build And Verification

```bash
cabal build
cabal run funq -- examples/bell00.funq
```

Run additional files in `examples/` when a change touches IR generation or
pipeline ordering.

## Design Constraints

- OpenQASM is the only active backend for now
- The middle end should remain backend-neutral so future backends such as QIR
  can reuse it
- `meas` is a consuming primitive: surface type `Qubit -> Bool`, but the input
  qubit is not available afterward
- The first qubit-allocation pass should use one statically assigned slot per
  `init`; no liveness-based slot reuse yet
- Recursive-function checking happens before closure conversion

## Key References

- `notes/cps-compilation-strategy.md`
- `notes/design-decisions.md`
- `notes/appel/index.md`
- `CLAUDE.md`
