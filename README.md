# FunQ

FunQ is a functional quantum programming language with linear types. It
currently targets OpenQASM, with the middle end structured so a future QIR
backend can reuse the same normalized IR.

The compiler is written in Haskell and follows the compilation pipeline described
in Appel's *Compiling with Continuations* (1992), adapted for quantum semantics.
See `notes/` for chapter-by-chapter notes on that adaptation.

---

## Language overview

FunQ looks like a small ML-family language. It has:

- **Linear types for qubits** — qubits cannot be copied or discarded implicitly,
  enforcing the no-cloning theorem at compile time
- **Quantum primitives** — `init`, `hgate`, `xgate`, `zgate`, `cnot`, `meas`
  built into the language
- **Higher-order functions** — functions are first-class; continuations and
  classically-controlled gates are expressible directly
- **Pattern matching** — tuples, ADTs, wildcards
- **Algebraic data types** — `data` declarations with constructors
- **Recursion** — general recursion supported; termination not checked

### Built-in types

| Type | Description |
|---|---|
| `Qubit` | Linear quantum bit — must be used exactly once |
| `Bool` | Classical boolean |
| `Int` | Classical integer |
| `String` | String literal |
| `Unit` | Unit value `()` |
| `(a, b)` | Tuple |
| `a -> b` | Function |

### Built-in operations

| Name | Type | Description |
|---|---|---|
| `init` | `Unit -> Qubit` | Allocate a fresh `\|0⟩` qubit |
| `meas` | `Qubit -> Bool` | Measure and collapse a qubit |
| `hgate` | `Qubit -> Qubit` | Hadamard gate |
| `xgate` | `Qubit -> Qubit` | Pauli-X (NOT) gate |
| `zgate` | `Qubit -> Qubit` | Pauli-Z gate |
| `cnot` | `Qubit -> Qubit -> (Qubit, Qubit)` | Controlled-NOT gate |

### Example: Bell state

```
module bell00 where

bell00 : Unit -> (Qubit, Qubit)
bell00 x =
  let
    a = init ()
    b = init ()
  in (cnot (hgate a) b)

output : (Bool, Bool)
output =
  let
    (a, b) = bell00 ()
  in (meas a, meas b)
```

### Example: Teleportation

```
module tele where

cctrl : (Qubit -> Qubit) -> (Bool -> Qubit -> Qubit)
cctrl g = (\ a phi -> (if a then (g phi) else phi))

tele : Qubit -> Qubit
tele phi =
  let
    (a, b) = bell00 ()
    (phi, a) = cnot phi a
    phi = hgate phi
    a = cctrl xgate (meas b) a
    phi' = cctrl zgate (meas a) phi
  in phi'
```

---

## Project structure

```
FunQ.cabal              — package definition and build configuration
src/
  Main.hs               — entry point: orchestrates full pipeline, emits OpenQASM
  Parser.hs             — Parsec parser (concrete syntax -> ConcreteSyntax)
  ConcreteSyntax.hs     — concrete syntax tree
  Resolve.hs            — scope resolution (ConcreteSyntax -> Syntax)
  Syntax.hs             — abstract syntax (uses nominal library for binding)
  LambdaIR.hs           — Lambda IR datatype (LExp); analogous to Appel's lexp
  Lower.hs              — lowering pass (Syntax -> LambdaIR)
  CPSExp.hs             — CPS IR datatype (CExp, Value); Appel §2.1
  ToCPS.hs              — CPS conversion pass (LambdaIR -> CPSExp); Appel Ch 5
  RecElim.hs            — recursion check: rejects recursive local CFix groups
  BoundedRecursion.hs   — recognizes bounded top-level self-recursive functions
  RecordShape.hs        — whole-module record-shape inference
  ModuleRecordFlatten.hs — interface record flattening before closure conversion
  GateDef.hs            — conservative gate/def classification
  ClosureConv.hs        — closure conversion pass (CPSExp -> CPSExp); Appel Ch 10
  Defunc.hs             — defunctionalization pass (CPSExp -> CPSExp)
  QubitHoist.hs         — static qubit-slot assignment after defunctionalization
  RecordFlatten.hs      — local record cleanup after qubit hoisting
  CompilePipeline.hs    — module-level compilation orchestration
  OpenQASM.hs           — OpenQASM 3.0 emitter (entrypoint-driven)
  TopMonad.hs           — top-level compilation monad
  Utils.hs              — shared utilities
examples/               — sample FunQ programs
notes/                  — compiler design notes
  appel/                — chapter-by-chapter notes on Appel (1992)
  cps-compilation-strategy.md — high-level CPS pipeline design
resources/              — reference PDFs (git-ignored)
```

---

## Setup

**Requirements**: GHC >= 9.10, cabal-install >= 3.12.

```bash
# Build
cabal build

# Run on a source file
cabal run funq -- examples/bell00.funq

# Run all examples
for f in examples/*.funq; do
  echo "=== $f ===" && cabal run funq -- "$f"
done
```

The compiler runs a full pipeline from source to OpenQASM 3.0 and prints only
the generated OpenQASM to stdout.

---

## Development status

| Stage | Status |
|---|---|
| Parse + concrete syntax | Done |
| Scope resolution | Done |
| Lower to Lambda IR (Appel Ch 4) | Done |
| Type checking (linear types) | Deferred |
| CPS conversion (Appel Ch 5) | Done |
| CPS optimisation | Deferred |
| Closure conversion (Appel Ch 10) | Done |
| Defunctionalization | Done |
| Qubit hoisting | Done |
| Tuple/record flattening | Done (interface + local) |
| Gate/def classification | Done (conservative first pass) |
| Bounded recursion (unroll) | Done (first cut) |
| Register spilling | Deferred |
| OpenQASM emission | Done (first cut; inlines from `output`) |

See `notes/appel/index.md` for the mapping of remaining stages to Appel chapters.
