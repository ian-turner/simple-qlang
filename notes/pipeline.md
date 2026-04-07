# FunQ Compilation Pipeline

Reference: *Compiling with Continuations*, Andrew W. Appel (Cambridge University Press, 1992)

See also: [quantum-semantics.md](quantum-semantics.md), [design-decisions.md](design-decisions.md), [appel/index.md](appel/index.md)

---

## What is Continuation-Passing Style?

CPS is a program representation in which:
- Every intermediate value is explicitly named (§1.1, p. 2)
- Functions never return — instead they call a *continuation* (a function
  representing "what to do next") with their result
- All function calls are therefore **tail calls** — there is no implicit
  runtime stack
- Control flow and data flow are both made fully explicit

Appel's key insight (§1.1, p. 3): once you name all control points,
optimizations like tail-call elimination fall out naturally as simple
substitutions on the CPS expression tree.

---

## Why CPS is a Good IR for FunQ

### Control and data flow are explicit (§1.2, p. 4)
CPS makes every aspect of control and data flow explicit. For quantum
compilation this matters: an OpenQASM program is a flat, ordered sequence of
gate applications and conditional branches. CPS naturally produces this
structure because all calls are tail calls — no stack to unwind, just a chain
of continuations.

### Single-binding property (§1.2, pp. 4–5)
CPS variables are bound exactly once, giving CPS a property similar to Static
Single-Assignment (SSA) form. This maps well to quantum circuits, where qubit
identities are tracked statically through the circuit.

### In-line expansion works cleanly (§1.2, p. 4)
Because all arguments to CPS functions are atomic (variables or constants,
never compound subexpressions), substitution is safe: it cannot change
evaluation order or duplicate effects.

### Closure conversion eliminates free variables (§1.4, p. 9; Ch 10)
The closure-conversion phase produces a CPS expression where every function is
closed (no free variables). This is required for the fully closed backend path,
even though the current OpenQASM emitter still reads the earlier
interface-flattened CPS.

---

## Backend Boundary

OpenQASM is the only active backend. The pipeline stays target-neutral through
the middle end so a future QIR backend can reuse the same normalized IR.

- CPS conversion, recursion handling, closure conversion, and
  defunctionalization are **backend-independent middle-end passes**
- OpenQASM-specific constraints are introduced as late as possible
- Backend-specific passes begin with qubit declaration strategy, gate/def
  classification, and final emission

---

## The CPS Datatype (§2.1, pp. 11–15)

FunQ's CPS IR is defined in `src/CPSExp.hs`. The constructors that map to Appel are:

| Constructor | Meaning | FunQ use |
|---|---|---|
| `CApp(f, [a1,...,ak])` | Tail call — no return | Gate application, continuation call |
| `CFix([(f,params,body),...], E)` | Mutually recursive definitions | Quantum subroutines, join continuations |
| `CPrimOp(op, vals, vars, conts)` | Primitive with continuations | Gates, measurement, arithmetic |
| `CSwitch(val, [E0,E1,...])` | Multiway branch | Branching on measurement outcomes |
| `CRecord(fields, w, E)` | Heap allocation | Tuple construction |
| `CSelect(i, v, w, E)` | Field selection | Tuple projection |

The key structural rule (§2.1, p. 13): **arguments to all CPS operations are
atomic** — variables or constants, never compound subexpressions.

---

## Mapping Quantum Concepts to CPS

### Quantum gates → `CPrimOp`

A single-qubit gate like `H q` becomes a `CPrimOp` that takes `q` as input,
binds the transformed qubit to a fresh variable, and continues:

```
PRIMOP(H, [VAR q], [q'], [E])
```

### Measurement → single-result `CPrimOp`

Measurement consumes its qubit and produces a classical `Bool`. In CPS:

```
PRIMOP(measure, [VAR q], [b], [cont])
```

`b` is the classical result; any subsequent branching happens through a
separate `CSwitch` rather than at the measurement site. See
[quantum-semantics.md](quantum-semantics.md) for the rationale.

### Classical control flow → `CSwitch`

`if/else` on a measured bit maps to:

```
SWITCH(VAR b, [E_false, E_true])
```

The emitter renders a two-arm `CSwitch` with a classical scrutinee as an
OpenQASM `if/else`. Larger fanouts emit as `switch`.

### Named join continuations for `switch`

`ToCPS.hs` introduces a named join continuation `j` around each `LSwitch`
translation so that the post-switch continuation code appears once in the CPS
tree rather than being duplicated into each arm:

```
FIX [(j, [x], c(x))]
  (SWITCH v [arm_i calls j])
```

See [passes/cps-conversion.md](passes/cps-conversion.md) for details.

### Recursive/parameterized circuits → `CFix`

Recursive quantum algorithms use `CFix`. Before emission, all recursion must be
eliminated or compiled to a `while` loop. See
[passes/recursion.md](passes/recursion.md).

---

## Compilation Pipeline

The current pipeline intentionally interleaves backend-neutral normalization
with a few OpenQASM-driven passes. In particular, gate/def classification runs
early on the interface-flattened CPS so it can inspect clean callable
interfaces before closure conversion adds records and indirect calls.

### 1. Parse + scope resolve **[done]**

FunQ source → annotated AST. Linear type checking is deferred; input is assumed
well-typed and linear.

Modules: `Parser.hs`, `ConcreteSyntax.hs`, `Resolve.hs`, `Syntax.hs`

### 2. Lower to λ-calculus form **[done]** (Appel Ch 4)

Desugars pattern matching, case expressions, and data constructors into a
minimal λ-calculus-like form. Multi-arg lambdas become nested `LLam`; let-
bindings become `LApp (LLam ...)`; constructor vars extracted via
`LDecon`/`LSelect`.

Modules: `LambdaIR.hs`, `Lower.hs`

### 3. CPS conversion **[done]** (Appel Ch 5)

Every function gains an extra continuation argument `c`. Returning a value `v`
becomes `APP(c, [v])`. Measurement becomes a single-result `PRIMOP`.

`LSwitch` gets a named join continuation so the post-switch code appears once.

Modules: `ToCPS.hs` — see [passes/cps-conversion.md](passes/cps-conversion.md)

### 4. Recursion check + tail-loop recognition **[done]**

Two sub-passes run in sequence:

**Local CFix check (`RecElim.hs`):** rejects mutual recursion in any
multi-function `CFix` group. Single-function self-recursion passes through.

**Top-level label check (`CompilePipeline.hs`):** builds a call graph over
compiled declarations; marks every declaration in a cyclic SCC as a recursion
error.

**Tail-loop recognition (`BoundedRecursion.hs`):** identifies self-recursive
declarations where every recursive call passes the outer continuation unchanged.
These become `while` loops at emission time (`isTailLoop` in `OpenQASM.hs`).
All other self-recursive declarations fall back to guarded inline expansion with
a 1000-call depth limit.

See [passes/recursion.md](passes/recursion.md).

### 5. Record-shape analysis + interface flattening **[done]**

**Shape analysis (`RecordShape.hs`):** infers whether each callable parameter
slot carries a scalar, a known-layout record, or an unknown/opaque value. Runs
on post-recursion CPS before closure conversion, while function interfaces are
still unobscured by closure records.

**Interface flattening (`ModuleRecordFlatten.hs`):** rewrites `CFix` parameter
lists and matching `CApp` argument lists when the shape is fully scalar. Handles
cross-declaration continuation-result flow so tuple outputs flatten across
declaration boundaries.

See [passes/record-flattening.md](passes/record-flattening.md).

### 6. Gate/def classification **[done]**

Classifies each top-level declaration as `gate` (pure unitary) or `def`
(contains measurement or classical control). Runs here so it sees the clean
pre-closure interfaces rather than closure records and dispatch scaffolding.

Module: `GateDef.hs` — see [passes/gate-def-classification.md](passes/gate-def-classification.md)

### 7. Closure conversion **[done]** (Appel Ch 10)

Eliminates all free variables. Every `CFix`-bound function becomes a closed
value; its free variables are bundled into an explicit closure record. After
this pass the program is one flat top-level `CFix` with no nested scopes.

Module: `ClosureConv.hs` — see [passes/closure-conversion.md](passes/closure-conversion.md) and [appel/ch10-closure-conversion.md](appel/ch10-closure-conversion.md)

### 8. Defunctionalization **[done]**

OpenQASM has no function values. After closure conversion, closures are still
first-class data. Defunctionalization converts all remaining higher-order
functions into integer tags + a top-level dispatch function.

Module: `Defunc.hs` — see [passes/defunctionalization.md](passes/defunctionalization.md)

### 9. Qubit hoisting **[done]**

OpenQASM requires all qubit declarations at top-level scope. This pass assigns
each `init` a static slot index, replaces the dynamic `PRIMOP(init, ...)` with
`VQubit i`, and returns the total qubit count for the emitter to declare.

Module: `QubitHoist.hs` — see [passes/qubit-hoisting.md](passes/qubit-hoisting.md)

### 10. Local record flattening **[done]**

Removes record traffic that remains within a single CPS expression after the
earlier interface-flattening pass. Closure-conversion and defunctionalization
records remain conservative.

Module: `RecordFlatten.hs` — see [passes/record-flattening.md](passes/record-flattening.md)

### 11. OpenQASM emission **[done — first cut]**

Entrypoint-driven structural translation from the interface-flattened CPS.
Starts from `output`, inlines reachable top-level calls, and emits one flat
OpenQASM program.

| CPS | OpenQASM |
|---|---|
| `APP` of a gate `PRIMOP` | gate application statement |
| `PRIMOP(measure, [q], [c], [E])` | `bit c = measure q;` then continue in `E` |
| `SWITCH(v, [E0, E1])` | `if (v == 1) { E1 } else { E0 }` |
| `SWITCH(v, [E0, E1, ...])` | `switch (v) { case 0: E0; ... }` |
| Self-recursive tail loop | OpenQASM 3.0 `while` loop |
| Top-level `output` binding | `output` declaration |

Module: `OpenQASM.hs` — see [passes/openqasm-emission.md](passes/openqasm-emission.md)

### Planned: static list erasure + bounded recursion IR

For recursive programs that operate on statically-sized lists (`ghz.funq`,
`qft_n.funq` with dynamic n), a future pass should:
- infer finite list shapes via `StaticShape.hs`
- lower bounded recursion into explicit loop IR
- erase statically-sized `List` values into fixed records/tuples

Currently handled by budget-unrolling in the emitter. See
[future/bounded-recursion.md](future/bounded-recursion.md).

---

## Optional Optimizations (deferred)

These improve output quality but are not required for correctness. Implement
after the required pipeline is working end-to-end.

| Pass | Appel | Benefit |
|---|---|---|
| β-contraction + constant folding | Ch 6 §6.1 | Eliminates trivial CPS wrappers |
| η-reduction | Ch 6 §6.2 | Removes redundant continuation wrappers |
| Beta expansion (inlining) | Ch 7 | Inlines small quantum subroutines |
| Hoisting | Ch 8 | Lifts `FIX` definitions |
| CSE | Ch 9 | Deduplicates classical sub-computations |
| Register spilling | Ch 11 | Only if live qubit count exceeds hardware limit |

Note: β-contraction must respect linearity — a qubit-valued binding may not be
inlined at more than one use site.

---

## Key Differences from Classical CPS Compilation

| Classical | Quantum (FunQ) |
|---|---|
| Free to copy/discard variables | Qubits are **linear** — used exactly once |
| Closure conversion is an optimization | FunQ keeps a required closed-CPS path, though today's emitter still reads earlier interface-flattened CPS |
| Function values survive to emit | **No function values in OpenQASM** — defunctionalization required |
| Recursion allowed at runtime | **No runtime recursion** — tail loops → `while`; others rejected |
| No qubit allocation constraints | All qubits must be **hoisted to top-level scope** |
| Tuples are a natural IR construct | **No tuple type** — must flatten before emit |

---

## Worked Example: `bell00`

### Source

```funq
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

### CPS conversion of `bell00`

Each function gains an extra argument `c` — its continuation.

```
FIX([(bell00, [x, c],
       PRIMOP(init,  [],              [a],      [
       PRIMOP(init,  [],              [b],      [
       PRIMOP(hgate, [VAR a],         [a'],     [
       PRIMOP(cnot,  [VAR a', VAR b], [q1, q2], [
       RECORD([(VAR q1, OFFp 0), (VAR q2, OFFp 0)], r,
         APP(VAR c, [VAR r]))
       ])])])])],
E)
```

`RECORD` allocates the qubit pair as `r`, then `r` is passed to the
continuation `c`.

### CPS conversion of `output`

After the interface-flattening pass, the tuple result from `bell00` is passed
as two scalar arguments. Measurement is a single-result primitive, not a
branching primitive:

```
APP(VAR bell00, [UNIT, FUN (a, b) ->
  PRIMOP(meas, [VAR a], [c0], [
  PRIMOP(meas, [VAR b], [c1], [
    output c0, c1
  ])])])
```

### After qubit hoisting + tuple flattening → OpenQASM

```openqasm
qubit[2] q;

reset q[0]; reset q[1];
h q[0];
cx q[0], q[1];
bit c0 = measure q[0];
bit c1 = measure q[1];
bit[2] output;
output[0] = c0;
output[1] = c1;
```

### Key observations

- **Gate `PRIMOP`**: one continuation — pure sequencing, no branching
- **Measurement `PRIMOP`**: one classical result, one continuation → `bit c = measure q;`
- **Nesting depth** of continuations encodes circuit depth and qubit liveness
- **No stack**: every call is a tail call (`APP`); after all passes the CPS
  expression is a flat sequence of OpenQASM statements
