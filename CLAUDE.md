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
| `RecElim.hs` | Recursion check: errors on mutual recursion in multi-function CFix groups; single-function self-recursion is allowed through |
| `BoundedRecursion.hs` | Top-level function helpers for recursion handling; exposes `extractTopLevelFunction` for the emitter and counted-recursion scaffolding for future lowering |
| `RecordShape.hs` | Whole-module tuple/data-flow record-shape analysis |
| `ModuleRecordFlatten.hs` | Interprocedural interface flattening before closure conversion |
| `CPSAtom.hs` | Shared atom/environment helpers used by both record-flattening passes |
| `GateDef.hs` | Conservative top-level gate/`def` classification |
| `CompilePipeline.hs` | Orchestrates per-declaration and whole-module compilation passes; produces `CompiledModule` |
| `OpenQASM.hs` | First entrypoint-driven OpenQASM emitter |
| `ClosureConv.hs` | Closure conversion: eliminates free variables (Appel Ch 10) |
| `Defunc.hs` | Defunctionalization: replaces runtime code pointers with tags and dispatch |
| `QubitHoist.hs` | Hoists `init` to static qubit slots after defunctionalization |
| `RecordFlatten.hs` | Local record simplification after qubit hoisting |
| `StaticShape.hs` | Whole-module list-size / aggregate shape inference scaffold; not currently wired into `CompilePipeline.hs` |
| `Main.hs` | Entry point: parses CLI args, calls `compileModule`, prints emitted OpenQASM, and can dump intermediate IR with `--debug` |

## Key dependencies

- `nominal` — provides `Atom`/`Variable` and `Bind` for capture-avoiding substitution in `Syntax.hs`
- `parsec` + `indents` — used together in `Parser.hs` for indentation-sensitive parsing
- `mtl` — monad transformers used in `TopMonad.hs`

---

## Project goal

FunQ is a functional quantum language with planned linear types that currently
targets OpenQASM. The middle end is intended to stay reusable for a future QIR
backend. The planned compilation pipeline follows Appel's *Compiling with
Continuations* (1992). See `notes/pipeline.md` for the full pipeline and `notes/index.md` for the
knowledge index.

**Current priority**: complete all required stages end-to-end before
introducing any optional optimizations or the linear type checker.

### Required stages (in order)
1. Parse + scope resolve — *done*
2. Lower to λ-calculus IR (Appel Ch 4) — *done*
3. CPS conversion (Appel Ch 5) — *done*
4. Recursion elimination — *done*. Local mutual recursion (multi-function CFix
   groups) is rejected. Single-function self-recursion passes through to the
   emitter (see stage 10), where tail loops become `while` loops and other
   self-recursive shapes fall back to guarded inline expansion (depth 1000).
5. Closure conversion (Appel Ch 10) — *done*
6. Defunctionalization — *done*
7. Qubit hoisting — *done*
8. Tuple/record flattening — *done* for tuple/data-flow records
9. Gate/def classification — *done* as a conservative top-level pass
10. Emit OpenQASM — *done* via entrypoint-driven emission, including:
    - Homogeneous output arrays and boolean two-arm `if/else` rendering
    - Self-recursive functions compiled as OpenQASM 3.0 `while` loops.
      `isTailLoop` (in `OpenQASM.hs`) decides the strategy: if every recursive
      call passes the continuation through unchanged (directly or via η-trivial
      chains, including Appel's curried-application intermediates), the function
      is a tail loop and gets a `while` loop. Otherwise it falls back to guarded
      inline expansion (depth limit: 1000) with a compile-time error if the
      limit is reached. This handles both bounded loops and probabilistic
      repeat-until-success circuits.

### Deferred until after the required pipeline works
- Linear type checking (assumes well-typed, linear input for now)
- CPS optimizations: β-contraction, η-reduction, inlining, CSE (Appel Chs 6–9)
- Register/qubit spilling (Appel Ch 11)

See `notes/resources/appel/index.md` for the mapping of stages to Appel chapters.

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

## Knowledge system

The `notes/` directory is a structured knowledge library. Use it as follows:

**Before making changes:**
- Read `notes/index.md` for orientation
- Read the relevant `notes/passes/<pass>.md` note before touching any pass
- Read `notes/design-decisions.md` and `notes/quantum-semantics.md` before
  making architectural choices

**After making changes:**
- Update the relevant pass note if behavior, invariants, or ordering changed
- Update `notes/pipeline.md` if the pipeline structure changed
- Update `notes/future/<topic>.md` if you implemented a planned item
- Remove or correct any note that now contradicts the code

Notes must stay accurate and concise. Do not leave a note saying something is
"planned" or a "limitation" if you just implemented or fixed it.

## Other notes

- The `resources/` directory is git-ignored (contains reference PDFs).
- Branch `claude-staging` is the working branch; `master` is the main branch.
