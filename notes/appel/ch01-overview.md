# Chapter 1 — Overview

Pages 1–10. Status: **complete**.

---

## 1.1 Continuation-Passing Style (pp. 1–4)

CPS is a program notation where:
- Every intermediate value is explicitly named
- Functions never return — they call a continuation with their result
- All function calls are therefore tail calls; no runtime stack is needed
- Control flow and data flow are both made fully explicit

### The `prodprimes` example (p. 2–3)

Appel converts a simple recursive ML function into CPS by naming all control
points (`k`, `j`, `h`) and all data labels (`b`, `p`, `a`, `m`, `q`, `i`).
The function gains an extra argument `c` — its continuation.

The key optimisation that falls out immediately: the trivial continuation
`h(q) = c(q)` can be eta-reduced away, replacing every call to `h` with a
direct call to `c`. This is tail-recursion elimination performed as a simple
substitution on the CPS tree (p. 3).

### Relevance to FunQ

- Every FunQ function will gain a continuation argument `c` during CPS conversion
- "Returning" a value `v` becomes `APP(c, [v])`
- Tail-recursive quantum algorithms (e.g. iterated gate application) eta-reduce cleanly

---

## 1.2 Advantages of CPS (pp. 4–6)

Appel compares CPS against four alternatives: λ-calculus, QUAD (register
transfers), PDG (program-dependence graphs), and SSA (static single-assignment).

| Property | CPS | λ | QUAD | PDG | SSA |
|---|---|---|---|---|---|
| In-line expansion | good | poor* | ok | ok | ok |
| Closure representations | good | good | poor | poor | poor |
| Dataflow analysis | good | poor | good | good | good |
| Register allocation | good | ok | good | ok | good |
| Vectorizing | — | — | — | good | — |
| Instruction scheduling | — | — | — | — | — |

*λ-calculus β-reduction is unsafe in strict call-by-value languages (p. 4):
substituting a non-trivial expression for a formal parameter can change
termination, evaluation count, and side-effect ordering. In CPS all arguments
are atomic (variables/constants), so substitution is always safe.

### Relevance to FunQ

- **In-line expansion**: safe β-reduction is important for inlining quantum
  subroutines (e.g. the `bell00` helper inside `tele`)
- **Closure representations**: FunQ has nested functions; CPS handles closure
  conversion naturally (Ch 10)
- **Dataflow / register allocation**: qubit liveness analysis (which qubit is
  "live" between its `init` and its `meas`) is a form of dataflow analysis;
  CPS makes this straightforward
- **SSA similarity**: CPS's single-binding property means each qubit variable
  is bound exactly once — matches the no-cloning constraint naturally

---

## 1.3 What is ML? (pp. 6–9)

Appel summarises ML's key properties relevant to the compiler:
- Strict (call-by-value) evaluation
- Higher-order functions with nested scope
- Parametric polymorphic types, statically checked
- Garbage collection
- Immutable data structures (aliasing problem disappears — p. 8–9)
- Formally defined semantics — any optimisation preserving the computable
  function is legal (p. 9)

### Relevance to FunQ

FunQ shares most of these properties. The critical difference:
- **Qubits are linear** — they cannot be copied or discarded (no-cloning theorem)
- This means the aliasing problem does *not* disappear for qubits; a qubit
  variable must be used exactly once
- The formally defined semantics point (p. 9) is valuable: any CPS
  transformation that preserves the quantum circuit semantics is legal, giving
  the optimizer freedom — but the optimizer must additionally preserve linearity

---

## 1.4 Compiler Organization (pp. 9–10)

Appel's SML/NJ pipeline (9 phases):

1. Lex, parse, type-check → annotated AST
2. Lower to λ-calculus-like representation (Ch 4)
3. Convert to CPS (Ch 5)
4. Optimize CPS (Chs 6–9)
5. Closure conversion (Ch 10)
6. Eliminate nested scopes → flat mutually recursive functions (Ch 10)
7. Register spilling (Ch 11)
8. Generate abstract-machine instructions (Ch 13)
9. Instruction scheduling, jump-size optimization, machine-code generation (Ch 14)

### FunQ adaptation

Phases 1–7 map directly. Phases 8–9 are replaced by:

8. Emit OpenQASM gate sequence / conditional branches
   - `PRIMOP(gate, ...)` → gate instruction
   - `PRIMOP(meas, ...)` with two conts → measurement + conditional branch
   - `FIX` of a subroutine → `gate` definition (OpenQASM) or function (QIR)
9. (Optional) OpenQASM/QIR-level optimisation passes (gate cancellation, etc.)
