# CFG Backend Rewrite Plan

## Motivation

The current `OpenQASM.hs` backend is a compile-time interpreter: it walks the
post-pipeline CPS using a stateful `EmitM` monad (a `StateT EmitState (Either
String)`), threading an in-memory `Env` of `ValueRep` bindings, and emits a
single flat QASM statement sequence. No top-level `gate`/`def` definitions are
ever produced. Non-tail-recursive functions hit a 1000-step inline depth limit.

The rewrite introduces a structured CFG IR between the CPS pipeline and QASM
emission, so that:
- Every lifted function becomes a top-level QASM `gate` or `def` definition.
- Self-recursion compiles to a `while` loop *inside* a named definition, not
  inlined into the flat program body.
- The inline depth limit is eliminated.
- The emitter becomes a stateless/pure renderer, not an interpreter.

## Why the Post-Pipeline CPS Is Ready

After the full pipeline (ClosureConv → Defunc → QubitHoist → RecordFlatten):

- **One top-level `CFix`** containing all lifted functions (ClosureConv lifts
  all nested `CFix` groups to module scope).
- **All calls are direct**: `CApp (VLabel l) args` — no higher-order calls, no
  closures, no free variables in function bodies.
- **All qubits are concrete slots**: `VQubit i` after QubitHoist.
- **`HoistedProgram.hoistedQubitCount`** gives the total qubit count.

At this point the CPS *is* a CFG in disguise: `CFix` functions are basic
blocks, `CApp` is a jump, `CSwitch` is a conditional branch, `CPrimOp` is a
statement. Making that structure explicit is the goal.

## New Modules

### `src/CFG.hs` — CFG IR datatypes

A structured (not flat-basic-block) IR that maps directly to QASM's structured
control flow (`if`/`else`, `while`, `switch`).

```
IRType      — Qubit | Int | Float | Bool | Bit | Unit | Record [IRType] | Label
IRAtom      — Var | QubitSlot Int | IntConst | FloatConst | BitConst | UnitVal
IRBinding   — Prim | Meas | Select | Offset | Record
IRStmt      — Let | Gate | TailCall | If | Switch | While
IRWhileUpdate — (param_name, new_atom)  [per-iteration classical update]
IRParam     — (name, IRType)
IRFunction  — name, CallableKind, params, body [IRStmt], isLoop
CFGModule   — functions, qubitCount, entryPoint, outputLayout
```

`IRLabel` represents defunctionalized integer dispatch tags — not qubits, not
classical values, just integer tags used as function dispatch selectors.

`IRWhile cond body updates` encodes everything the emitter needs to render a
while loop: the emitter assigns mutable temporaries for each classical parameter
and emits the update assignments before the loop-exit check.

### `src/ToCFG.hs` — CPS → CFG lowering

Entry point:
```haskell
toCFGModule :: CompiledModule -> Either String CFGModule
```

Reads `compiledFlattenedIR`, `compiledHoistedIR`, and `compiledCallableKinds`
from the existing `CompiledModule` fields — no pipeline changes needed.

Key sub-passes:

**`extractCFix`** — matches `CFix defs (CApp (VLabel "halt") [VVar f])` and
returns the function list. Absorbs `BoundedRecursion.extractTopLevelFunction`.

**`inferParamTypes`** — use-site analysis: gate argument → `IRQubit`, switch
scrutinee → `IRLabel`, primop results typed by prim. Fixed-point across all
function bodies in the `CFix` group.

**`lowerBody`** — translates a CPS function body to `[IRStmt]`:
- `CPrimOp gate` → `IRGate`
- `CPrimOp PMeas` → `IRLet IRBit (IRBindMeas ...)`
- `CPrimOp classical` → `IRLet T (IRBindPrim ...)` (or constant-fold)
- `CSelect/CRecord/COffset` → `IRLet` or constant-fold via `selectAtom`
- `CApp (VLabel l) args` → `IRTailCall l args`
- `CSwitch v arms` → constant-fold if scrutinee known, else `IRIf`/`IRSwitch`
- `CFix` inside a body → lowering error (should not occur post-ClosureConv)

**Loop detection** — `isTailLoop` and `compileBodyAsLoop` move from
`OpenQASM.hs` to `ToCFG.hs`. Called before lowering each function body. If the
function is a tail loop, `compileBodyAsLoop` produces an `IRWhile`; recursive
self-calls in the body become `IRWhileUpdate` entries.

**`inferOutputLayout`** — scans `IRTailCall "halt" args` in the `output`
function body, collects arg types, computes `IROutputLayout`. Replaces the
runtime `chooseOutputLayout` / `updateOutputLayout` logic.

## Modified Module

### `src/OpenQASM.hs` — pure renderer

```haskell
emitOpenQASM :: CompiledModule -> Either String String
emitOpenQASM cm = toCFGModule cm >>= pure . renderCFGModule
```

New rendering functions (pure, no monad):
- `renderCFGModule` — header, qubit decl, output decl, function defs, entry body
- `renderFunction` — emits `gate name q0, q1 { ... }` or `def name(params) -> T { ... }`
- `renderIRStmt` — recursive, indented

**Deleted** (~850 lines of interpreter infrastructure):
`EmitState`, `EmitM`, `Callable`, `ValueRep`, `ClassicalRep`, `OutputLayout`,
`runExp`, all `run*`/`apply*`/`eval*` functions, `emitBranchArm`,
`mergeBranchStates`, `splitCommonSuffix`, `isTailLoop`, `compileAsLoop`.

**Kept** (moved or adapted):
`renderClassicalType`, `renderClassicalPrim`, `evalClassicalPrimConst`,
`isBooleanPrim`, `isClassicalPrim`, `primArity`, `renderOutputDecls`.

## Deleted Module

### `src/BoundedRecursion.hs`
Only imported by `OpenQASM.hs`. Its `extractTopLevelFunction` is absorbed into
`ToCFG.extractCFix`. Delete entirely; remove from `FunQ.cabal`.

## Implementation Steps

Each step leaves the compiler buildable:

1. Create `src/CFG.hs` (types only); add CFG + ToCFG to `FunQ.cabal`
2. Create `src/ToCFG.hs` stub (`Left "not yet implemented"`)
3. Implement `extractCFix` and `inferParamTypes`
4. Implement `lowerBody` — non-branching case
5. Add `CSwitch` lowering to `lowerBody`
6. Move `isTailLoop`/`compileAsLoop` to ToCFG; wire into `lowerBody`
7. Implement `toCFGModule` assembly
8. Add `renderCFGModule` etc. to `OpenQASM.hs`; keep legacy as `emitOpenQASMLegacy`
9. Switch `emitOpenQASM` to CFG path; verify parity on all examples
10. Delete legacy interpreter; delete `BoundedRecursion.hs`; update cabal

## Verification

After step 9, diff QASM output against legacy path for:
```
examples/bell00.funq      -- basic gate + measurement
examples/countdown.funq   -- while-loop path
```

Expected structural differences: new output has top-level `gate`/`def` blocks
before the main body; entry point calls functions by name rather than inlining.
