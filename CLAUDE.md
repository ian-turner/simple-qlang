# CLAUDE.md

Development notes for Claude Code working on the FunQ compiler.

---

## Build and run

```bash
cabal build                              # build the compiler
cabal run funq -- examples/bell00.funq  # run on a source file
```

The compiler now runs through a first OpenQASM emission path and prints the
generated OpenQASM to stdout. There is no test suite yet; verify changes by
running the examples manually.

---

## Source module responsibilities

| Module | Role |
|---|---|
| `Parser.hs` | Parsec parser; produces `ConcreteSyntax` terms |
| `ConcreteSyntax.hs` | Concrete syntax tree (close to surface syntax) |
| `Resolve.hs` | Scope resolution: `ConcreteSyntax` → `Syntax`; resolves names |
| `Syntax.hs` | Abstract syntax tree using the `nominal` library for alpha-equivalence |
| `TopMonad.hs` | Top-level compilation monad (error handling, state threading) |
| `Utils.hs` | Shared utilities |
| `LambdaIR.hs` | Lambda IR datatype (`LExp`) — intermediate between AST and CPS |
| `Lower.hs` | Lowers resolved AST to `LExp` |
| `CPSExp.hs` | CPS IR datatype (`CExp`, `Value`, `AccessPath`) |
| `ToCPS.hs` | CPS conversion: `LExp` → `CExp` (Appel Ch 5) |
| `RecElim.hs` | Recursion check: errors on recursive `CFix` groups |
| `RecordShape.hs` | Whole-module tuple/data-flow record-shape analysis |
| `ModuleRecordFlatten.hs` | Interprocedural interface flattening before closure conversion |
| `GateDef.hs` | Conservative top-level gate/`def` classification |
| `OpenQASM.hs` | First entrypoint-driven OpenQASM emitter |
| `ClosureConv.hs` | Closure conversion: eliminates free variables (Appel Ch 10) |
| `Defunc.hs` | Defunctionalization: replaces runtime code pointers with tags and dispatch |
| `QubitHoist.hs` | Hoists `init` to static qubit slots after defunctionalization |
| `RecordFlatten.hs` | Local record simplification after qubit hoisting |
| `Main.hs` | Orchestrates the full pipeline and prints emitted OpenQASM |

## Key dependencies

- `nominal` — provides `Atom`/`Variable` and `Bind` for capture-avoiding substitution in `Syntax.hs`
- `parsec` + `indents` — used together in `Parser.hs` for indentation-sensitive parsing
- `mtl` — monad transformers used in `TopMonad.hs`

---

## Project goal

FunQ is a functional quantum language with planned linear types that currently
targets OpenQASM. The middle end is intended to stay reusable for a future QIR
backend. The planned compilation pipeline follows Appel's *Compiling with
Continuations* (1992). See `notes/cps-compilation-strategy.md` for the full
pipeline with required/optional classification.

**Current priority**: complete all required stages end-to-end before
introducing any optional optimizations or the linear type checker.

### Required stages (in order)
1. Parse + scope resolve — *done*
2. Lower to λ-calculus IR (Appel Ch 4) — *done*
3. CPS conversion (Appel Ch 5) — *done*
4. Recursion elimination — *done* for local recursion, with a first bounded
   top-level self-recursive subset handled by `src/BoundedRecursion.hs`
5. Closure conversion (Appel Ch 10) — *done*
6. Defunctionalization — *done*
7. Qubit hoisting — *done*
8. Tuple/record flattening — *done* for tuple/data-flow records
9. Gate/def classification — *done* as a conservative top-level pass
10. Emit OpenQASM — *first cut done* via entrypoint-driven emission, including
    homogeneous output arrays and boolean two-arm `if/else` rendering

### Deferred until after the required pipeline works
- Linear type checking (assumes well-typed, linear input for now)
- CPS optimizations: β-contraction, η-reduction, inlining, CSE (Appel Chs 6–9)
- Register/qubit spilling (Appel Ch 11)

See `notes/appel/index.md` for the mapping of stages to Appel chapters.

---

## Quantum semantics constraints

These affect every phase of the compiler and must not be violated:

- **No-cloning**: qubits cannot be duplicated. The linear type system enforces
  this; no pass may copy or alias a qubit variable.
- **No-discard**: qubits must be explicitly measured or passed on; they cannot
  be silently dropped.
- **No closure capture of qubits**: a qubit must not appear as a free variable
  of a function — it must be passed as an explicit argument. The type checker
  will enforce this; closure conversion can then follow Appel's algorithm unchanged.
- **No qubit spilling to memory**: qubits cannot be stored in heap records and
  reloaded. The spill phase applies only to classical (Bool/Int) variables;
  qubit width must be checked statically.
- **Measurement produces a classical result**: `meas` lowers to a single-result
  CPS `PRIMOP` whose continuation receives a `Bool`-like classical value.
  Any later branching on that value is represented explicitly with `CSwitch`.

---

## Notes

- The `resources/` directory is git-ignored (contains reference PDFs).
- The `notes/` directory contains compiler design notes — keep them updated
  when making significant design decisions.
- Branch `claude-staging` is the working branch; `master` is the main branch.
