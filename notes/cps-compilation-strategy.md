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

### Measurement → `PRIMOP` with two continuations

Measurement is the crucial case. Unlike a gate, measurement is **non-deterministic**: it produces a classical bit and branches on the outcome. This maps directly to `PRIMOP` with two continuation expressions (§2.1, p. 13, the `>` comparison example):

```
PRIMOP(measure, [VAR q], [], [E_zero, E_one])
```

`E_zero` and `E_one` are the continuations for the `|0⟩` and `|1⟩` outcomes respectively. In OpenQASM this becomes:

```openqasm
bit c = measure q;
if (c) { E_one } else { E_zero }
```

### Classical control flow → `SWITCH`

`if/else` on classical bits (e.g., after measurement) maps directly to `SWITCH`:

```
SWITCH(VAR bit, [E_false, E_true])
```

### Recursive/parameterized circuits → `FIX`

Recursive quantum algorithms (QFT, quantum walk, etc.) use `FIX` to define mutually recursive functions. Before emission, all recursion must be eliminated (see Recursion Elimination below).

---

## Compilation Pipeline

Following Appel's compiler organization (§1.4, pp. 9–10), adapted for FunQ and the OpenQASM target:

### 1. Parse + type-check
FunQ source → annotated AST.
- Linear type checking ensures qubits are not cloned or discarded (no-cloning theorem)
- This is FunQ-specific and not covered by Appel

### 2. Lower to λ-calculus form (Chapter 4)
- Desugar pattern matching, case expressions, data constructors
- FunQ already has data types and case expressions

### 3. Convert to CPS (Chapter 5)
- Every function gains an extra continuation argument `c`
- Returning a value `v` becomes `APP(c, [v])`
- Measurement becomes `PRIMOP(measure, ...)` with two continuations
- See §5.4 (function calls) and §5.7 (case statements) for the conversion rules

### 4. Optimize CPS (Chapters 6–9)
- Constant folding and β-contraction (§6.1): inline trivial continuations
- Eta reduction (§6.2): eliminate redundant continuation wrappers
- Common subexpression elimination (Chapter 9): deduplicate classical computations

### 5. Closure conversion (Chapter 10)
Eliminate all free variables from functions. Every `FIX`-bound function becomes a closed value; its environment is passed as an explicit extra argument. Required before the remaining passes, which operate on closed terms.

### 6. Recursion elimination
**OpenQASM has no general recursion.** All recursive `FIX` bodies must be eliminated before emission. Two cases:

- **Statically bounded recursion**: if the recursion depth is a compile-time constant or an `input` parameter, unroll or lower to an OpenQASM `for` loop:
  ```openqasm
  for int i in [0:1:n-1] { body }
  ```
- **Unbounded recursion**: a **compile-time error**. FunQ programs with unbounded recursion cannot be compiled to OpenQASM. The programmer must supply a bound (e.g., via a type-level or expression-level annotation) or restructure the algorithm.

Recursion elimination is done before defunctionalization because unrolling may eliminate some higher-order function uses.

### 7. Defunctionalization
**OpenQASM has no function values.** Closure conversion eliminates free variables but still produces closures as first-class data. Defunctionalization (Reynolds-style) converts all remaining higher-order functions into tagged data + a top-level dispatch function, eliminating function values entirely.

This is required for FunQ programs that pass functions as arguments — e.g.:
```funq
cctrl g = \a phi -> if a then g phi else phi
```
Here `g` is a function-valued argument. After defunctionalization, `g` becomes a tag (an integer or constructor), and the dispatch function performs the appropriate gate call based on the tag.

### 8. Qubit hoisting
**OpenQASM requires all qubit declarations at top-level scope** — they are not allowed inside subroutines or conditional branches. FunQ's `init()` creates qubits dynamically inside expressions.

This pass:
1. Statically counts all qubit allocations in the program
2. Emits `qubit q_i;` declarations at program scope
3. Replaces each `PRIMOP(init, [], [q], [E])` with a `reset q_i;` statement that reuses the pre-declared qubit

### 9. Tuple flattening
**OpenQASM has no tuple type.** The CPS IR uses `RECORD`/`SELECT` for tuple construction and projection (e.g., qubit pairs returned from two-qubit gates like `cnot`). This pass replaces every tuple with a set of individual scalar variables and rewrites all `SELECT` projections as direct variable references.

### 10. Gate/def classification
Before emission, classify each closed `FIX`-bound function as either a `gate` or a `def`:

- **`gate`**: the body contains only gate applications (no measurement, no classical variables, no classical control flow). Parameters are angles only. Use this for pure unitary operations:
  ```openqasm
  gate my_gate(theta) q { U(theta, 0, 0) q; }
  ```
- **`def`**: the body contains measurement, classical control flow, or classical variables. Takes qubits by reference and can return a classical value:
  ```openqasm
  def xmeasure(qubit q) -> bit { H q; return measure q; }
  ```

This classification is a concrete analysis step: traverse the body and check for `PRIMOP(measure,...)` or `SWITCH` nodes. If any are present, emit `def`; otherwise emit `gate`.

### 11. Emit OpenQASM
After the preceding passes the CPS expression is a flat graph of tail calls with no free variables, no function values, no tuples, and all qubits globally declared. Emission is a structural translation:

| CPS | OpenQASM |
|---|---|
| `APP` of a gate `PRIMOP` | gate application statement |
| `PRIMOP(measure, [q], [], [E0, E1])` | `bit c = measure q; if (c) { E1 } else { E0 }` |
| `SWITCH(v, [E0, E1, ...])` | `switch (v) { case 0: E0; case 1: E1; ... }` |
| `FIX` of a pure unitary | `gate` definition |
| `FIX` of a subroutine with measurement | `def` definition |
| Top-level `output` binding | `output` declaration |
| Classical `PRIMOP(+, ...)` etc. | arithmetic expression |

Type sizing for emission: FunQ's `Int` → `int[32]`, `Float` → `float[64]`, `Bool` → `bool`.

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

The linearity constraint (no-cloning) means the optimizer must not beta-expand qubit-valued expressions more than once. This requires a linearity check during the optimization passes in step 4.

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
       PRIMOP(init,  [],              [a],
       PRIMOP(init,  [],              [b],
       PRIMOP(hgate, [VAR a],         [a'],
       PRIMOP(cnot,  [VAR a', VAR b], [q1, q2],
       APP(VAR c, [RECORD([VAR q1, VAR q2])])
       )))))],
E)
```

### CPS Conversion of `output` — Measurement Branches

```
APP(VAR bell00, [UNIT, FUN (a, b) ->
  PRIMOP(meas, [VAR a], [],
    [ (* outcome |0> *)
      PRIMOP(meas, [VAR b], [],
        [ APP(VAR k, [RECORD([FALSE, FALSE])]),   (* b = |0> *)
          APP(VAR k, [RECORD([FALSE, TRUE])]) ]), (* b = |1> *)
      (* outcome |1> *)
      PRIMOP(meas, [VAR b], [],
        [ APP(VAR k, [RECORD([TRUE, FALSE])]),    (* b = |0> *)
          APP(VAR k, [RECORD([TRUE, TRUE])]) ])   (* b = |1> *)
    ])])
```

### After Qubit Hoisting + Tuple Flattening

After hoisting, the two `PRIMOP(init)` calls become resets of global qubits. After tuple flattening, `RECORD([q1, q2])` and `SELECT` are replaced by direct variable references.

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
output bit[2] result;
result[0] = c0;
result[1] = c1;
```

### Key Observations

- **Gate `PRIMOP`**: one continuation — pure sequencing, no branching
- **Measurement `PRIMOP`**: two continuations — maps to `bit c = measure q; if (c) { ... } else { ... }`
- **Nesting depth** of continuations directly encodes circuit depth and qubit liveness
- **No stack**: every call is a tail call (`APP`), so after all passes the CPS expression is a flat sequence of OpenQASM statements

---

## Next Steps / Chapters to Read

- **Chapter 2** (§2.1–2.5): Full CPS datatype, closure conversion sketch, spilling
- **Chapter 5**: The CPS conversion algorithm
- **Chapter 6**: Optimizations — β-contraction (§6.1) and eta reduction (§6.2)
- **Chapter 10**: Closure conversion algorithm in detail
