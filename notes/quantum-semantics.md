# Quantum Semantics Constraints

These constraints arise from quantum physics and affect every phase of the
compiler. Violations produce incorrect circuits, not just compilation errors.

See also [design-decisions.md](design-decisions.md) for decisions that flow
from these constraints.

---

## No-cloning

Qubits cannot be duplicated. If `q` is a qubit, the program may not produce
two references to `q` (or two identically-valued qubits derived from `q`)
without an explicit entangling operation.

**Compiler enforcement:** The future linear type checker will statically reject
programs that use a qubit variable more than once. Until the type checker
exists, the compiler assumes well-typed, linear input.

**Effect on passes:**
- CPS conversion must not beta-expand qubit-valued bindings at more than one
  use site.
- Closure conversion and defunctionalization must not copy qubit values through
  closure records.
- Record flattening must propagate `VQubit` leaves rather than copying them.

---

## No-discard

Qubits must be explicitly measured or passed on. They cannot be silently
dropped.

**Compiler enforcement:** Future linear type checker. Currently assumed.

---

## No closure capture of qubits

A qubit must not appear as a free variable of a function — it must be passed as
an explicit argument. This is a consequence of no-cloning: if a function
captures a qubit in its closure, the closure record would hold an alias.

**Effect on passes:** Closure conversion can follow Appel's algorithm unchanged
because well-typed FunQ programs never have qubit free variables. The no-qubit-
capture invariant is what makes Appel's closure conversion safe for FunQ.

---

## No qubit spilling to memory

Qubits cannot be stored in heap records and reloaded. The spill phase (Appel
Ch 11) applies only to classical (`Bool`/`Int`) variables. Qubit register width
must be checked statically.

**Effect on passes:** Qubit hoisting assigns one static slot per `init`.
Record flattening must recognize `VQubit` leaves and preserve them without
wrapping them in heap records.

---

## Measurement consumes its qubit

`meas : Qubit -> Bool`. The input qubit is consumed — it cannot be used after
the call.

In CPS, measurement is a single-result primitive:

```
PRIMOP(measure, [VAR q], [b], [cont])
```

where `b` receives the classical `Bool` result. Any subsequent branching on
`b` goes through an explicit `CSwitch` rather than being forced at the
measurement site. In OpenQASM this emits as:

```openqasm
bit b = measure q[i];
```

**Why single-result (not two-continuation):** An earlier encoding used two
continuations `[zeroCont, oneCont]` to model non-determinism. That was
abandoned because it forced a backend branch at every measurement, duplicating
downstream code even when the program only wanted a `Bool` value and might
branch later — or not at all.

---

## Quantum types vs classical types

| Type | Representation | Notes |
|---|---|---|
| `Qubit` | `VQubit Int` after hoisting | Static slot assigned by `QubitHoist.hs` |
| `Bool` | `VBool` / `bit` | Measurement result; classical control |
| `Int` | `VInt` / `int[32]` | Classical arithmetic |
| `Float` | Symbolic string | Preserved as text through middle end; see [design-decisions.md](design-decisions.md) |
| `Unit` | `VUnit` | Argument placeholder |
| `(A, B)` | `CRecord` / flattened | Flattened to scalars before emission |
| `List a` | Tagged `CRecord` | Must be erased before emission (see [future/bounded-recursion.md](future/bounded-recursion.md)) |

---

## OpenQASM constraints that follow from quantum semantics

| Constraint | Source | How FunQ handles it |
|---|---|---|
| All qubit declarations at top scope | OpenQASM 3.0 | Qubit hoisting pass |
| No tuple type | OpenQASM 3.0 | Record/tuple flattening |
| No function values | OpenQASM 3.0 | Defunctionalization |
| No general recursion | OpenQASM 3.0 | Recursion check; tail loops → `while` |
| No algebraic list type | OpenQASM 3.0 | Static list erasure (planned) |
