# CLAUDE.md

Development notes for Claude Code working on the FunQ compiler.

---

## Build and run

```bash
cabal build                              # build the compiler
cabal run funq -- examples/bell00.funq  # run on a source file
```

The compiler currently prints the resolved abstract syntax tree to stdout.
There is no test suite yet — verify changes by running the examples manually.

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
| `Main.hs` | Orchestrates parse → resolve → print |

## Key dependencies

- `nominal` — provides `Atom`/`Variable` and `Bind` for capture-avoiding substitution in `Syntax.hs`
- `parsec` + `indents` — used together in `Parser.hs` for indentation-sensitive parsing
- `mtl` — monad transformers used in `TopMonad.hs`

---

## Project goal

FunQ is a functional quantum language with linear types that compiles to
OpenQASM / QIR. The planned compilation pipeline (not yet implemented beyond
scope resolution) follows Appel's *Compiling with Continuations* (1992):

1. Parse + scope resolve *(done)*
2. Type check with linear types for qubits
3. Lower to λ-calculus IR (Appel Ch 4)
4. Convert to CPS (Appel Ch 5)
5. Optimise CPS: β-contraction, η-reduction, inlining (Appel Chs 6–9)
6. Closure conversion (Appel Ch 10)
7. Register/qubit spilling (Appel Ch 11)
8. Emit OpenQASM or QIR

See `notes/appel/index.md` for chapter-by-chapter notes on the pipeline,
and `notes/cps-compilation-strategy.md` for the high-level design.

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
- **Measurement is branching**: `meas` produces two continuations in CPS
  (`|0⟩` branch and `|1⟩` branch), not a single result, analogous to a
  two-way `PRIMOP` in Appel's notation.

---

## Notes

- The `resources/` directory is git-ignored (contains reference PDFs).
- The `notes/` directory contains compiler design notes — keep them updated
  when making significant design decisions.
- Branch `claude-staging` is the working branch; `master` is the main branch.
