# FunQ Knowledge Index

FunQ is a functional quantum language that compiles to OpenQASM 3.0. The middle
end follows Appel's *Compiling with Continuations* (1992); the backend targets
OpenQASM with a planned QIR backend reusing the same normalized IR.

---

## Quick navigation

| Topic | File |
|---|---|
| Full pipeline — theory, stages, worked examples | [pipeline.md](pipeline.md) |
| Quantum constraints that affect every pass | [quantum-semantics.md](quantum-semantics.md) |
| Key design decisions | [design-decisions.md](design-decisions.md) |
| Known bugs and limitations | [issues.md](issues.md) |
| CPS conversion details and join continuations | [passes/cps-conversion.md](passes/cps-conversion.md) |
| Recursion — local rejection, top-level check, while loops | [passes/recursion.md](passes/recursion.md) |
| Record/tuple flattening — shape analysis and rewriting | [passes/record-flattening.md](passes/record-flattening.md) |
| Closure conversion — shared flat closures and lifting | [passes/closure-conversion.md](passes/closure-conversion.md) |
| Defunctionalization — eliminating function values | [passes/defunctionalization.md](passes/defunctionalization.md) |
| Qubit hoisting — static slot assignment | [passes/qubit-hoisting.md](passes/qubit-hoisting.md) |
| Gate/def classification | [passes/gate-def-classification.md](passes/gate-def-classification.md) |
| OpenQASM emitter — architecture and design notes | [passes/openqasm-emission.md](passes/openqasm-emission.md) |
| Static shape analysis — list sizes and parametric shapes | [passes/static-shape.md](passes/static-shape.md) |
| Bounded recursion lowering — future plan | [future/bounded-recursion.md](future/bounded-recursion.md) |
| Backend refactor — CPS→SSA reading map | [future/backend-refactor.md](future/backend-refactor.md) |
| Resource hub — reading list and paper notes | [resources/index.md](resources/index.md) |
| Appel chapter notes | [resources/appel/index.md](resources/appel/index.md) |

---

## Source module map

| Module | Role |
|---|---|
| `Parser.hs` | Parsec parser; produces `ConcreteSyntax` terms |
| `ConcreteSyntax.hs` | Concrete syntax tree (close to surface syntax) |
| `Resolve.hs` | Scope resolution: `ConcreteSyntax` → `Syntax` |
| `Syntax.hs` | AST using the `nominal` library for alpha-equivalence |
| `TopMonad.hs` | Top-level compilation monad |
| `Utils.hs` | Shared utilities, including fresh variable generation and variable/debug rendering |
| `LambdaIR.hs` | Lambda IR datatype (`LExp`) |
| `Lower.hs` | Lowers resolved AST to `LExp` |
| `CPSExp.hs` | CPS IR datatype (`CExp`, `Value`, `AccessPath`) |
| `ToCPS.hs` | CPS conversion: `LExp` → `CExp` (Appel Ch 5) |
| `RecElim.hs` | Rejects mutual recursion in multi-function CFix groups; self-recursion passes through |
| `BoundedRecursion.hs` | Top-level recursion helpers; exposes `extractTopLevelFunction` for the emitter and counted-recursion scaffolding for future lowering |
| `RecordShape.hs` | Whole-module tuple/data-flow record-shape analysis |
| `ModuleRecordFlatten.hs` | Interprocedural interface flattening before closure conversion |
| `CPSAtom.hs` | Shared atom/environment helpers |
| `GateDef.hs` | Conservative top-level gate/`def` classification |
| `CompilePipeline.hs` | Orchestrates per-declaration and whole-module passes; produces `CompiledModule` |
| `OpenQASM.hs` | Entrypoint-driven OpenQASM emitter |
| `ClosureConv.hs` | Closure conversion: eliminates free variables (Appel Ch 10) |
| `Defunc.hs` | Defunctionalization: replaces runtime code pointers with tags and dispatch |
| `QubitHoist.hs` | Hoists `init` to static qubit slots |
| `RecordFlatten.hs` | Local record simplification after qubit hoisting |
| `StaticShape.hs` | Whole-module list-size / aggregate shape inference scaffold; not currently wired into `CompilePipeline.hs` |
| `Main.hs` | Entry point: parses CLI args, calls `compileModule`, prints OpenQASM, and can dump intermediate IR with `--debug` |

---

## Pipeline stages at a glance

| # | Stage | Module(s) | Status |
|---|---|---|---|
| 1 | Parse + scope resolve | `Parser.hs`, `Resolve.hs` | done |
| 2 | Lower to Lambda IR | `Lower.hs` | done |
| 3 | CPS conversion | `ToCPS.hs` | done |
| 4 | Recursion checks + emitter-side tail-loop compilation | `RecElim.hs`, `CompilePipeline.hs`, `BoundedRecursion.hs`, `OpenQASM.hs` | done (first cut) |
| 5 | Record-shape analysis + interface flattening | `RecordShape.hs`, `ModuleRecordFlatten.hs` | done |
| 6 | Gate/def classification | `GateDef.hs` | done |
| 7 | Closure conversion | `ClosureConv.hs` | done |
| 8 | Defunctionalization | `Defunc.hs` | done |
| 9 | Qubit hoisting | `QubitHoist.hs` | done |
| 10 | Local record flattening | `RecordFlatten.hs` | done |
| 11 | OpenQASM emission | `OpenQASM.hs` | done (first cut) |
| — | Static list erasure + bounded recursion IR | planned (`StaticShape.hs`, `CFor`, and `VQubitArr` scaffolding exist; end-to-end wiring is still pending) | see [future/bounded-recursion.md](future/bounded-recursion.md) |
| — | CPS optimizations (β, η, inlining, CSE) | deferred | Appel Ch 6–9 |
| — | Linear type checking | deferred | assumes well-typed input |

---

## Examples

| File | Description |
|---|---|
| `bell00.funq` | Bell state preparation; tuple output flattening |
| `tele.funq` | Quantum teleportation; classically controlled gates, higher-order functions |
| `ghz.funq` | GHZ state; list recursion via budget-unrolling |
| `qft_n.funq` | n-qubit QFT; list recursion, controlled phase gates |
| `rus.funq` | Repeat-until-success; self-recursive tail loop → `while` |
| `countdown.funq` | Self-recursive integer countdown → `while` |
| `float_constants.funq` | Float and `pi` literals preserved symbolically to emission |
| `ops.funq` | Classical arithmetic and comparison operators |
| `list.funq` | Structural list operations (concat, reverse) |

---

## Build

```bash
cabal build
cabal run funq -- examples/bell00.funq
cabal run funq -- --debug examples/bell00.funq
```
