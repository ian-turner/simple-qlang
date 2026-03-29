# CPS Compilation Strategy for FunQ → OpenQASM/QIR

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
CPS makes every aspect of control and data flow explicit in the program representation. For quantum compilation this matters: an OpenQASM or QIR program is essentially a flat, ordered sequence of gate applications and conditional branches. CPS naturally produces this structure because all calls are tail calls — there is no "stack" to unwind, just a chain of continuations.

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

`E_zero` and `E_one` are the continuations for the `|0⟩` and `|1⟩` outcomes respectively. In OpenQASM this becomes a conditional branch on a measurement result; in QIR it becomes a `__quantum__rt__result_equal` check followed by a branch instruction.

### Classical control flow → `SWITCH`

`if/else` on classical bits (e.g., after measurement) maps directly to `SWITCH`:

```
SWITCH(VAR bit, [E_false, E_true])
```

### Recursive/parameterized circuits → `FIX`

Recursive quantum algorithms (QFT, quantum walk, etc.) use `FIX` to define mutually recursive functions. The compiler can then decide whether to inline them (beta expansion, Chapter 7) or emit them as subroutines in the target language.

---

## Suggested Compilation Pipeline

Following Appel's compiler organization (§1.4, pp. 9–10), adapted for FunQ:

1. **Parse + type-check** FunQ source → annotated AST
   - Linear type checking ensures qubits are not cloned or discarded (no-cloning theorem)
   - This is FunQ-specific and not covered by Appel

2. **Lower to λ-calculus form** (Chapter 4 in Appel)
   - Desugar pattern matching, case expressions, data constructors
   - FunQ already has data types and case expressions

3. **Convert to CPS** (Chapter 5)
   - Every function gains an extra continuation argument `c`
   - Returning a value `v` becomes `APP(c, [v])`
   - Measurement becomes `PRIMOP(measure, ...)` with two continuations
   - See §5.4 (function calls) and §5.7 (case statements) for the conversion rules

4. **Optimize CPS** (Chapters 6–9)
   - Constant folding and β-contraction (§6.1): inline trivial continuations
   - Eta reduction (§6.2): eliminate redundant continuation wrappers
   - Common subexpression elimination (Chapter 9): deduplicate classical computations

5. **Closure conversion** (Chapter 10)
   - Eliminate all free variables from functions
   - Required before emitting flat target code

6. **Emit OpenQASM / QIR**
   - After closure conversion and register spilling (Chapter 11), the CPS expression is a flat graph of tail calls
   - `APP` of a gate → emit gate instruction
   - `PRIMOP(measure,...)` → emit measurement + branch
   - `FIX` of a subroutine → emit a callable gate definition (OpenQASM `gate` or QIR function)
   - `SWITCH` → emit classical `if` (OpenQASM) or LLVM branch (QIR)

---

## Key Differences from Classical CPS Compilation

| Classical | Quantum (FunQ) |
|---|---|
| All `PRIMOP`s have one continuation | `measure` has **two** continuations |
| Free to copy/discard variables | Qubits are **linear** — must be used exactly once |
| Closure conversion is an optimization | Closure conversion is **required** before emit |
| `FIX` bodies may be recursive freely | Recursive circuit unrolling may require depth bounds |

The linearity constraint (no-cloning) means the optimizer must not beta-expand qubit-valued expressions more than once. This requires a linearity check during the optimization passes in step 4.

---

## Next Steps / Chapters to Read

- **Chapter 2** (§2.1–2.5): Full CPS datatype, closure conversion sketch, spilling — understand the complete IR before implementing it
- **Chapter 5**: The CPS conversion algorithm — this is what we implement to translate FunQ AST → CPS
- **Chapter 6**: Optimizations — especially β-contraction (§6.1) and eta reduction (§6.2), which simplify away trivial continuation wrappers generated by the conversion
- **Chapter 10**: Closure conversion algorithm in detail — necessary to produce flat, emittable code
