# CPS Compilation Strategy for FunQ → OpenQASM

Reference: *Compiling with Continuations*, Andrew W. Appel (Cambridge University Press, 1992)

---

## What is Continuation-Passing Style?

CPS is a program representation in which:
- Every intermediate value is explicitly named (§1.1, p. 2)
- Functions never return — instead they call a *continuation* (a function representing "what to do next") with their result
- All function calls are therefore **tail calls** — there is no implicit runtime stack
- Control flow and data flow are both made fully explicit

Appel's key insight (§1.1, p. 3): once you name all control points, optimizations like tail-call elimination fall out naturally as simple substitutions on the CPS expression tree.

---

## Why CPS is a Good IR for FunQ

### Control and data flow are explicit (§1.2, p. 4)
CPS makes every aspect of control and data flow explicit in the program representation. For quantum compilation this matters: an OpenQASM program is essentially a flat, ordered sequence of gate applications and conditional branches. CPS naturally produces this structure because all calls are tail calls — there is no "stack" to unwind, just a chain of continuations.

### Single-binding property (§1.2, pp. 4–5)
CPS variables are bound exactly once, giving CPS a property similar to Static Single-Assignment (SSA) form. This maps well to quantum circuits, where qubit identities are tracked statically through the circuit.

### In-line expansion works cleanly (§1.2, p. 4)
Because all arguments to CPS functions are atomic (variables or constants, never subexpressions — see §2.1, p. 13), substitution is safe: it cannot change evaluation order or duplicate effects. Classical sub-computations in FunQ can be inlined freely.

### Closure conversion eliminates free variables (§1.4, p. 9; Chapter 10)
The pipeline's closure-conversion phase produces a CPS expression where every function is closed (no free variables). This is necessary before emitting flat target code like OpenQASM, where there is no notion of lexical scope.

---

## Backend Boundary

OpenQASM is the only active backend for now, but the pipeline should remain as
target-neutral as possible until the final lowering stages. In particular:

- Parsing, lowering to Lambda IR, CPS conversion, recursion handling, closure
  conversion, and defunctionalization are **backend-independent middle-end
  passes**
- OpenQASM-specific constraints should be introduced as late as possible
- Backend-specific passes begin with target lowering decisions such as qubit
  declaration strategy, `gate` vs `def` classification, and final emission

This keeps the IR useful for future backends such as QIR without forcing the
front half of the compiler to track OpenQASM-specific surface restrictions.

---

## The CPS Datatype (§2.1, pp. 11–15)

Appel defines a concrete ML datatype `cexp` as the IR. The constructors relevant to FunQ are:

| Constructor | Meaning | Relevance to FunQ |
|---|---|---|
| `APP(f, [a1,...,ak])` | Tail call — no return | Every gate application and qubit operation |
| `FIX([(f,params,body),...], E)` | Mutually recursive function definitions | Quantum subroutines, recursive circuits |
| `PRIMOP(op, vals, vars, conts)` | Primitive operation with continuations | Gates, measurement, arithmetic on classical bits |
| `SWITCH(val, [E0,E1,...])` | Multiway branch on an integer | Branching on measurement outcomes |
| `RECORD(fields, w, E)` | Heap allocation | Qubit register allocation |
| `SELECT(i, v, w, E)` | Field selection | Selecting individual qubits from a register |

The key structural rule (§2.1, p. 13): **arguments to all CPS operations are atomic** — they are variables or constants, never compound subexpressions. This means CPS has already "scheduled" all intermediate computations by naming them.

---

## Mapping Quantum Concepts to CPS

### Quantum gates → `PRIMOP`

A single-qubit gate like `H q` becomes a `PRIMOP` that takes `q` as an argument, binds the (transformed) qubit to a fresh variable, and continues:

```
PRIMOP(H, [VAR q], [q'], [E])
```

This mirrors Appel's arithmetic example (§2.1, p. 13): `a + b` yielding `c`, continuing with `E`, is `PRIMOP(+, [VAR a, VAR b], [c], [E])`.

### Measurement → single-result `PRIMOP`

Measurement is the crucial case. Unlike a gate, measurement is
**non-deterministic**: it produces a classical bit. Measurement also
**consumes** its qubit operand: after `meas q`, the qubit `q` is no longer
available to the program. The surface type remains `Qubit -> Bool`, and the
current CPS lowering now preserves that directly by treating measurement as an
ordinary single-result primitive:

```
PRIMOP(measure, [VAR q], [b], [E])
```

Here `b` is the measured classical result passed into continuation `E`. Any
later branching on that result is expressed explicitly through `SWITCH`, which
avoids forcing an OpenQASM branch at every measurement site. In OpenQASM this
becomes:

```openqasm
bit c = measure q;
```

### Classical control flow → `SWITCH`

`if/else` on classical bits (e.g., after measurement) maps directly to `SWITCH`:

```
SWITCH(VAR bit, [E_false, E_true])
```

The current OpenQASM emitter still lowers that CPS shape, but when the branch is
dynamic and has exactly two arms it renders the result as `if/else` instead of
`switch`.

### Recursive/parameterized circuits → `FIX`

Recursive quantum algorithms (QFT, quantum walk, etc.) use `FIX` to define mutually recursive functions. Before emission, all recursion must be eliminated (see Recursion Elimination below).

---

## Compilation Pipeline

Following Appel's compiler organization (§1.4, pp. 9–10), adapted for FunQ and
currently lowered to OpenQASM. Stages 1–6 are intended to remain largely
backend-neutral; later stages capture backend-specific constraints.

Each stage is marked **[required]** (needed to produce valid OpenQASM) or
**[optional]** (improves output quality but can be skipped without affecting
correctness). The current priority is to complete all required stages first.

### 1. Parse + scope resolve **[required, done]**
FunQ source → annotated AST. Linear type checking is deferred until after the
full OpenQASM pipeline is working; for now we assume a well-typed, linear input.

### 2. Lower to λ-calculus form **[required, done]** (Chapter 4)
Desugar pattern matching, case expressions, and data constructors into a
minimal λ-calculus-like form that the CPS conversion algorithm (Ch 5) operates
on. FunQ already has these constructs; this pass simplifies them into the
small set of forms Appel's converter expects.

Implemented in `src/LambdaIR.hs` (the `LExp` datatype) and `src/Lower.hs`
(the `Syntax.Exp → LExp` transformation). Key design points:
- Multi-arg lambdas → nested `LLam`; `let`/`let (a,b) =` → `LApp (LLam ...)`
- Case patterns flattened: constructor vars extracted via `LDecon`/`LSelect`
- Multi-field constructors (`Cons x xs'`) → `LCon "Cons" (LTuple [x, xs'])`
- Primitives (`hgate`, `+`, etc.) → `LPrim op []` as first-class values;
  curried applications eagerly collected into `LPrim op [args]`

### 3. Convert to CPS **[required, done]** (Chapter 5)
- Every function gains an extra continuation argument `c`
- Returning a value `v` becomes `APP(c, [v])`
- Measurement becomes `PRIMOP(measure, ...)` with one classical result and one
  continuation

### 4. Bounded shape inference **[required for bounded recursion, planned]**
Bounded recursion over lists cannot be lowered safely unless the compiler knows
the recursive data has a finite compile-time shape. A new shape/size analysis
stage should therefore run immediately after CPS conversion while recursive
structure is still visible.

Expected responsibilities:

- infer fixed-size list lengths where possible
- recognize top-level declarations whose result length is a known integer or a
  symbolic size parameter
- record enough facts to drive bounded recursion lowering

Examples:

- `init_n n` produces a `List Qubit` of length `n`
- `meas_all xs` preserves the input list length
- `ghz_4` has concrete length `4`

This analysis should stay conservative: anything not proven bounded remains
`unknown` and must still be rejected later if it participates in recursion.

### 5. Recursion elimination / bounded lowering **[required, partially done]**
**OpenQASM has no general recursion.** All recursive `FIX` bodies must be
eliminated before emission. Two cases:

- **Statically bounded recursion**: unroll or lower to an OpenQASM `for` loop:
  ```openqasm
  for int i in [0:1:n-1] { body }
  ```
- **Unbounded recursion**: a **compile-time error**. The programmer must
  supply a bound or restructure the algorithm.

This pass runs on CPS **before closure conversion**. That ordering is
intentional: recursive calls are still direct `CApp (VVar f) args` nodes with
`f` bound in the enclosing `CFix`, so recursion is easy to detect and is the
right place to lower bounded recursion into explicit loop IR. After closure
conversion, the same call is rewritten through closure records and code-pointer
selection, which obscures direct self/sibling calls and makes the analysis
substantially more complex without helping the current backend.

Implemented in `src/RecElim.hs` (`elimRecursion :: CExp -> Either String CExp`).
Detection: for each `CFix` group, intersect the bound names with the set of
callee variables found anywhere in the function bodies; a non-empty
intersection is a compile-time error.

Current status: detection and error reporting are complete. `src/CompilePipeline.hs`
also performs a module-level top-level recursion check over `VLabel` calls
between compiled declarations so recursive label cycles are rejected before
OpenQASM emission.

Planned extension:

- replace reject-only recursion handling with a bounded-recursion lowering pass
- support counted self-recursion over `Int`
- support structural recursion over statically sized lists once shape inference
  can prove the length
- add an explicit loop node to the CPS IR so later passes and the emitter no
  longer need recursion to represent finite iteration

The GHZ example needs more than simple integer-counted loops. `init_n`,
`cnot_layer`, and `meas_all` all depend on static-list recursion lowering, so
GHZ support requires both bounded shape inference and bounded recursion
lowering.

First landed subset:

- top-level self-recursive unary declarations can now pass when they match the
  pattern `if n == 0 then base else self (n - 1)`
- expansion currently requires the counter to be a compile-time constant and is
  implemented as safe bounded unrolling in the emitter
- this is intentionally a first cut and does not yet address list recursion or
  dynamic loop bounds

### 6. Static list erasure **[required for bounded list recursion, planned]**
After bounded list recursion is lowered, statically sized `List` values should
be rewritten into fixed records or tuples before the later backend-facing
passes. OpenQASM does not have runtime algebraic lists, so `Nil`/`Cons`
structure must not survive to emission.

Expected responsibilities:

- erase fixed-size `List a` values into record-like aggregates
- rewrite list traversal helpers into indexed or record-field access
- leave dynamically sized lists unsupported

This stage is the bridge that should let examples like `ghz_4` compile once the
compiler has already proven the list sizes.

### 7. Closure conversion **[required, done]** (Chapter 10)
Eliminate all free variables. Every `FIX`-bound function becomes a closed
value; its free variables are bundled into an explicit closure record passed
as an extra argument. After this pass the program is one flat top-level `FIX`
with no nested scopes — a prerequisite for all subsequent passes.

### 8. Defunctionalization **[required, done]**
**OpenQASM has no function values.** After closure conversion, closures are
still first-class data. Defunctionalization (Reynolds-style) converts all
remaining higher-order functions into tagged data + a top-level dispatch
function, eliminating function values entirely.

Required for programs that pass functions as arguments — e.g.:
```funq
cctrl g = \a phi -> if a then g phi else phi
```
After defunctionalization, `g` becomes a tag (an integer or constructor) and
the dispatch function performs the appropriate gate call based on the tag.

Current status: implemented as a first correctness-oriented pass in
`src/Defunc.hs`. The pass replaces synthetic closure-conversion code labels
with integer tags in closure records and rewrites indirect closure calls into
`SWITCH`-based dispatch over direct `VLabel` calls. Dispatch sets are currently
per-declaration rather than minimized per call site.

### 9. Qubit hoisting **[required, done]**
**OpenQASM requires all qubit declarations at top-level scope.**
FunQ's `init()` creates qubits dynamically inside expressions.

This pass:
1. Statically counts all qubit allocations in the program
2. Records how many backend qubit slots later emission must declare
3. Replaces each `PRIMOP(init, [], [q], [E])` with the statically assigned
   slot `q_i`

Initial policy: **one slot per `init`**. This first version does not perform
liveness-based qubit reuse. A later optimisation pass may recycle slots once
qubit lifetimes are analysed precisely.

Current status: implemented in `src/QubitHoist.hs`. The pass rewrites `PInit`
sites after defunctionalization, substitutes the bound result variable with
`VQubit i`, and returns the total number of required qubit slots alongside the
rewritten CPS expression. Actual `qubit q_i;` declarations and `reset`
insertion remain part of the future OpenQASM emission stage.

### 10. Tuple/record flattening **[required, done for tuple/data-flow records]**
**OpenQASM has no tuple type.** The CPS IR uses `RECORD`/`SELECT` for tuples
(e.g., qubit pairs from `cnot`). This pass replaces each tuple with individual
scalar variables and rewrites all `SELECT` projections as direct variable references.

Current status: implemented in two layers. `src/ModuleRecordFlatten.hs`
performs shape-driven interface flattening before closure conversion, including
continuation-result flow across top-level declaration boundaries when the
record shape is known. `src/RecordFlatten.hs` then performs local record
cleanup after qubit hoisting. Closure-conversion and defunctionalization
records remain conservative by design.

### 11. Gate/def classification **[required, first cut done]**
Classify each closed `FIX`-bound function as `gate` or `def` before emission:

- **`gate`**: body contains only gate applications — no measurement, no classical
  control flow. Emitted as an OpenQASM `gate` definition.
  ```openqasm
  gate my_gate(theta) q { U(theta, 0, 0) q; }
  ```
- **`def`**: body contains measurement or classical control flow. Emitted as
  an OpenQASM `def`.
  ```openqasm
  def xmeasure(qubit q) -> bit { H q; return measure q; }
  ```

Current status: implemented conservatively in `src/GateDef.hs` as a top-level
module analysis over the interface-flattened CPS. The current classifier
provides stable `gate`/`def` summaries for top-level declarations without
letting closure-conversion or dispatch scaffolding dominate the result.

### 12. Emit OpenQASM **[required, first cut done]**
Structural translation from the flat, closed, recursion-free CPS:

| CPS | OpenQASM |
|---|---|
| `APP` of a gate `PRIMOP` | gate application statement |
| `PRIMOP(measure, [q], [c], [E])` | `bit c = measure q;` then continue in `E` |
| `SWITCH(v, [E0, E1])` | `if (v == 1) { E1 } else { E0 }` |
| `SWITCH(v, [E0, E1, ...])` | `switch (v) { case 0: E0; case 1: E1; ... }` |
| `FIX` of a pure unitary | `gate` definition |
| `FIX` of a subroutine with measurement | `def` definition |
| Top-level `output` binding | `output` declaration |
| Classical `PRIMOP(+, ...)` etc. | arithmetic expression |

Type sizing: FunQ's `Int` → `int[32]`, `Float` → `float[64]`, `Bool` → `bool`,
and measured classical results currently emit as `bit`.

Current status: a first emitter now lives in `src/OpenQASM.hs`. The current
backend is intentionally pragmatic: it emits from the interface-flattened CPS,
starts from `output`, performs global qubit allocation during emission, and
inlines reachable top-level calls into a single OpenQASM program. Reusable
`gate` / `def` declaration emission is still future backend refinement work.
Float literals are now preserved symbolically through the middle end so values
such as `pi` can reach OpenQASM emission unchanged; future backends such as QIR
should lower those symbolic constants at a backend-specific boundary rather
than in the front half of the compiler.
If earlier compilation stages reported recursion errors, the emitter now stops
immediately with that compile-stage failure summary instead of attempting
backend inlining through missing declarations.

---

## Optional Optimizations (post-pipeline)

These improve output quality but are not required for correctness. Implement
after the required pipeline is working end-to-end.

| Pass | Appel | Benefit |
|---|---|---|
| β-contraction + constant folding | Ch 6 §6.1 | Eliminates trivial CPS wrappers introduced during conversion; reduces output verbosity significantly |
| η-reduction | Ch 6 §6.2 | Removes redundant continuation wrappers |
| Beta expansion (inlining) | Ch 7 | Inlines small quantum subroutines; reduces `def` call overhead |
| Hoisting | Ch 8 | Lifts `FIX` definitions; improves code structure |
| Common subexpression elimination | Ch 9 | Deduplicates classical sub-computations |
| Register spilling | Ch 11 | Only needed if live qubit count exceeds hardware limit; classical variables spill to heap records |

Note: β-contraction must respect linearity — a qubit-valued binding may not be
inlined at more than one use site. This check is straightforward once the
linear type checker is in place.

---

## Key Differences from Classical CPS Compilation

| Classical | Quantum (FunQ) |
|---|---|
| All `PRIMOP`s have one continuation | `measure` has **two** continuations |
| Free to copy/discard variables | Qubits are **linear** — must be used exactly once |
| Closure conversion is an optimization | Closure conversion is **required** before emit |
| Function values survive to emit | **No function values in OpenQASM** — defunctionalization required |
| Recursion allowed at runtime | **No runtime recursion** — must eliminate statically |
| No qubit allocation constraints | All qubits must be **hoisted to top-level scope** |
| Tuples are a natural IR construct | **No tuple type** — must flatten before emit |

The linearity constraint (no-cloning) means the optimizer must not beta-expand qubit-valued expressions more than once. This will require a linearity check when the optional optimization passes are implemented.

---

## Worked Example: `bell00` and `output`

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

### CPS Conversion of `bell00` (§5.1–5.4)

Each function gains an extra argument `c` — the continuation to call with the result.

```
-- CPS version (pseudocode using FunQ-like syntax)
bell00 (x, c) =
  init ((), fun a ->
  init ((), fun b ->
  hgate (a, fun a' ->
  cnot (a', b, fun result ->
  c result))))
```

In Appel's `cexp` datatype (§2.1, pp. 11–12):

```
FIX([(bell00, [x, c],
       PRIMOP(init,  [],              [a],   [
       PRIMOP(init,  [],              [b],   [
       PRIMOP(hgate, [VAR a],         [a'],  [
       PRIMOP(cnot,  [VAR a', VAR b], [q1, q2], [
       RECORD([(VAR q1, OFFp 0), (VAR q2, OFFp 0)], r,
         APP(VAR c, [VAR r]))
       ])])])])],
E)
```

Note: `RECORD` is a `cexp` (it allocates, binds a result variable, and
continues) — it is not a value and cannot appear inline inside `APP`. The
qubit pair is first heap-allocated into record `r`, then `r` is passed to the
continuation `c`.

### CPS Conversion of `output` — Measurement Branches

```
APP(VAR bell00, [UNIT, FUN (r) ->
  SELECT(0, VAR r, a,
  SELECT(1, VAR r, b,
  PRIMOP(meas, [VAR a], [],
    [ (* outcome |0> *)
      PRIMOP(meas, [VAR b], [],
        [ RECORD([(INT 0, OFFp 0), (INT 0, OFFp 0)], res, APP(VAR k, [VAR res])),  (* b=|0> *)
          RECORD([(INT 0, OFFp 0), (INT 1, OFFp 0)], res, APP(VAR k, [VAR res])) ](* b=|1> *)
      ),
      (* outcome |1> *)
      PRIMOP(meas, [VAR b], [],
        [ RECORD([(INT 1, OFFp 0), (INT 0, OFFp 0)], res, APP(VAR k, [VAR res])),  (* b=|0> *)
          RECORD([(INT 1, OFFp 0), (INT 1, OFFp 0)], res, APP(VAR k, [VAR res])) ](* b=|1> *)
      )
    ])))
```

Note: `bell00`'s continuation receives a record `r` holding the qubit pair;
`SELECT` projects each qubit out before measuring. Boolean outcomes are
represented as `INT 0`/`INT 1` (or `VBool` in the FunQ datatype) and bundled
into a fresh record before being passed to the outer continuation `k`.

### After Qubit Hoisting + Tuple Flattening

After hoisting, the two `PRIMOP(init)` calls become resets of global qubits. After tuple flattening, the `RECORD`/`SELECT` pairs used for qubit tuples are replaced by direct variable references.

```openqasm
// Hoisted qubit declarations
qubit q0;
qubit q1;

// bell00 emitted as a def (returns classical tuple → two output bits)
// (or inlined after beta-contraction)

// output
reset q0; reset q1;
h q0;
cnot q0, q1;
bit c0 = measure q0;
bit c1 = measure q1;
bit[2] output;
output[0] = c0;
output[1] = c1;
```

### Key Observations

- **Gate `PRIMOP`**: one continuation — pure sequencing, no branching
- **Measurement `PRIMOP`**: one classical result and one continuation —
  maps to `bit c = measure q;`, with later `SWITCH` nodes providing any needed
  classical branching
- **Nesting depth** of continuations directly encodes circuit depth and qubit liveness
- **No stack**: every call is a tail call (`APP`), so after all passes the CPS expression is a flat sequence of OpenQASM statements

---

## Chapters to Read

Deferred (after required pipeline is complete):
- **Chapter 3**: Formal semantics of CPS — needed for reasoning about linearity preservation
- **Chapter 6**: Optimizations — β-contraction (§6.1) and eta reduction (§6.2)

Chapters 1, 2, 4, 5, 10, 11 are fully read; see `notes/appel/` for detailed notes.
