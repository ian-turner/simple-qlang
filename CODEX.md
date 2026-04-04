# CODEX.md

Codex-specific project context for FunQ.

---

## Knowledge system

The `notes/` directory is a structured knowledge library. Read and update it
alongside any non-trivial code change:

- **Read first:** `notes/index.md` (orientation), then the relevant
  `notes/passes/<pass>.md` note before touching pass code.
- **Update after:** if you change a pass's behavior, position, or invariants,
  update its note. If you resolve a "planned" item, update `notes/future/`.
  Notes must not contradict the code.

See `AGENTS.md` for the full knowledge-system policy.

---

## What matters most

- Preserve a clean separation between backend-neutral IR passes and
  OpenQASM-specific lowering
- Do not introduce backend assumptions early unless they are recorded as a
  deliberate design decision in `notes/design-decisions.md`
- When a design choice changes the pipeline or semantics, update the matching
  note in `notes/`

---

## Pipeline summary

| # | Stage | Module(s) |
|---|---|---|
| 1 | Parse + scope resolve | `Parser.hs`, `Resolve.hs` |
| 2 | Lower to Lambda IR | `Lower.hs` |
| 3 | CPS conversion | `ToCPS.hs` |
| 4 | Recursion check + tail-loop recognition | `RecElim.hs`, `BoundedRecursion.hs`, `CompilePipeline.hs` |
| 5 | Record-shape analysis + interface flattening | `RecordShape.hs`, `ModuleRecordFlatten.hs` |
| 6 | Gate/def classification | `GateDef.hs` |
| 7 | Closure conversion | `ClosureConv.hs` |
| 8 | Defunctionalization | `Defunc.hs` |
| 9 | Qubit hoisting | `QubitHoist.hs` |
| 10 | Local record flattening | `RecordFlatten.hs` |
| 11 | OpenQASM emission | `OpenQASM.hs` |

---

## Semantics to preserve

- Qubits are linear values and must not be duplicated or discarded silently
- Measurement consumes its qubit and yields a classical `Bool`; later branching
  on that value is expressed through a separate `CSwitch`, not at the
  measurement site
- Closure conversion must not capture qubits as free variables
- See `notes/quantum-semantics.md` for the full constraint list

---

## Practical workflow

1. Read `notes/index.md` and the relevant pass note before changing pipeline code
2. Read `notes/design-decisions.md` before making architectural choices
3. Verify with `cabal build` and at least one example run
4. Update the relevant notes file after the change

---

## Current gaps

- No type checker yet
- No automated test suite
- Emitter does not yet emit reusable `gate`/`def` declarations (inlines from `output`)
- Bounded recursion uses budget-unrolling; explicit loop IR and static-list
  erasure are future work (see `notes/future/bounded-recursion.md`)
- QIR is future scope only; keep current code focused on OpenQASM while
  preserving backend-neutral middle-end structure
