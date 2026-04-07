# AGENTS.md

Shared instructions for coding agents working in this repository.

---

## Knowledge system

The `notes/` directory is a structured knowledge library for this project.
**Always read and update it as part of any non-trivial code change.**

### Structure

```
notes/
├── index.md                  — top-level hub; start here
├── pipeline.md               — full CPS compilation pipeline with theory
├── quantum-semantics.md      — quantum constraints that affect every pass
├── design-decisions.md       — key design decisions with rationale
├── passes/                   — one note per compiler pass
│   ├── cps-conversion.md
│   ├── recursion.md
│   ├── static-shape.md
│   ├── record-flattening.md
│   ├── closure-conversion.md
│   ├── defunctionalization.md
│   ├── qubit-hoisting.md
│   ├── gate-def-classification.md
│   └── openqasm-emission.md
├── future/                   — planned work not yet implemented
│   ├── bounded-recursion.md
│   └── backend-refactor.md
├── resources/                — resource hub, Appel notes, and paper notes
│   ├── index.md
│   └── appel/
```

### When to read the notes

- **Before touching any pipeline code:** read `notes/index.md` to orient
  yourself, then read the `notes/passes/` note for the pass you are changing.
- **Before adding a new pass or stage:** read `notes/pipeline.md` to understand
  where the new pass fits and what invariants it must preserve.
- **Before making design decisions:** read `notes/design-decisions.md` and
  `notes/quantum-semantics.md` to check whether the decision has already been
  made and recorded.

### When to update the notes

Update the relevant note whenever you:
- Change the behavior or position of a pass in the pipeline
- Add, remove, or rename a module
- Make a design decision that affects multiple passes
- Resolve a "planned" item in `notes/future/`
- Discover that a note contains stale or incorrect information

**Do not leave notes in a state that contradicts the code.** If a note says
something is "planned" and you implement it, update the note to say it is done.
If a note says a limitation exists and you fix it, remove the limitation note.

Notes should be accurate and concise. Prefer updating an existing note over
creating a new one. When a note's purpose is complete (e.g., a plan that has
been fully executed), either remove it or reduce it to a brief historical
summary at the top of the relevant pass note.

---

## Working rules

- Prefer small, targeted edits over broad refactors
- Keep the pipeline documentation aligned with implementation when stage
  ordering or semantics change
- Validate changes by building and running the example programs:

```bash
cabal build
cabal run funq -- examples/bell00.funq
```

Run additional examples when a change touches IR generation or pipeline ordering.

---

## Current compiler state

- **Implemented:** parse, scope resolution, Lambda IR lowering, CPS conversion,
  recursion check + while-loop compilation, interface record flattening, gate/def
  classification, closure conversion, defunctionalization, qubit hoisting, local
  record flattening, first OpenQASM emission
- **Not yet implemented:** reusable `gate`/`def` emission; explicit bounded-
  recursion IR lowering; static list erasure
- **No automated test suite yet** — validate with examples

---

## Design constraints

- OpenQASM is the only active backend; keep the middle end backend-neutral
- `meas` is a consuming primitive: surface type `Qubit -> Bool`; input qubit
  is not available afterward
- Qubit allocation: one static slot per `init`; no liveness-based reuse yet
- Recursion checking happens before closure conversion
- See `notes/quantum-semantics.md` for the full set of constraints

---

## Key references

- `notes/index.md` — full knowledge index
- `notes/pipeline.md` — CPS pipeline design
- `notes/design-decisions.md` — design decisions
- `notes/resources/index.md` — resource hub and reading list
- `notes/resources/appel/index.md` — Appel chapter map
- `CLAUDE.md` — Claude Code–specific project notes
