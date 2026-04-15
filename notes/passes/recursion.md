# Recursion Check and Tail-Loop Compilation

**Modules:** `src/RecElim.hs`, `src/BoundedRecursion.hs`, `src/CompilePipeline.hs`, `src/OpenQASM.hs`, `src/QASMAnalysis.hs`

See also: [../pipeline.md](../pipeline.md), [../future/bounded-recursion.md](../future/bounded-recursion.md)

---

## Overview

OpenQASM has no general runtime recursion, and qubit arrays must be declared
with a static size. This means the compiler must statically determine the total
qubit count for any well-formed program. Self-recursive functions therefore fall
into exactly three classes:

### Class 1 — Static recursion → `for` loop

The trip count is a compile-time constant. Qubits may accumulate across
iterations because `total = count × allocs_per_iter` is statically known. The
counter argument at the call site must be a `VInt n` literal (or evaluable to
one via constant folding).

Example: `init_n 4` — 0 qubits in, 4 qubits out, trip count = 4.

### Class 2 — Dynamic recursion → `while` loop

The trip count is not statically known. The invariant that makes this safe is
**qubit neutrality**: every `init` in the loop body must be consumed by a `meas`
on that same qubit before the recursive call. The set of qubit arguments passed
to the recursive call must equal the set of qubit parameters of the function —
no new qubits escape to the next iteration. QubitHoist assigns one slot to each
`init` inside the body; physical reuse is valid because measurement collapses
the qubit to `|0>`.

Example: `rus_loop` — 1 qubit in, 1 qubit out; ancilla allocated and measured
within each iteration.

### Class 3 — Reject

Dynamic trip count **and** qubit accumulation. This would require an unbounded
qubit array — not representable in OpenQASM. Must produce a compile-time error
with a message that explains the constraint.

Example: `init_n x` where `x` is a runtime variable.

### Type-level encoding

With linear types, the class of a recursive function falls out from its type.
Define `qubits(T)` = number of qubit slots in type `T` (`Qubit` → 1, tuples sum,
`List a` of length n → `n × qubits(a)`, classical types → 0). For a function
`A → B`:
- `qubits(B) > qubits(A)` → qubits accumulate; requires static trip count → Class 1 only
- `qubits(B) = qubits(A)` → qubit-neutral; valid while-loop candidate → Class 2
- `qubits(B) < qubits(A)` → qubit-consuming; valid while-loop candidate

---

Three checks run in order before backend emission.

---

## 1. Local CFix recursion check (`RecElim.hs`)

For each `CFix` group, the pass intersects the set of bound function names with
the set of callee variables found anywhere in the function bodies. A non-empty
intersection means at least one function calls another (or itself) in the group.

- **Multi-function groups** with any recursion are **rejected** (mutual
  recursion is not supported).
- **Single-function groups** with self-recursion **pass through** — the
  self-recursive declaration proceeds to the top-level label check and
  ultimately to the emitter.

---

## 2. Top-level label cycle check (`CompilePipeline.hs`)

After per-declaration CPS lowering and the local recursion check,
`CompilePipeline.hs` performs a module-level analysis:

- Collects top-level `VLabel` callees from each declaration's CPS
- Builds a call graph over compiled declarations
- Marks declarations in cyclic strongly-connected components of size greater
  than 1 as recursion errors

This catches mutual top-level label cycles across declarations. Those cycles
would otherwise reach interface flattening, classification, and the emitter.

A single self-recursive top-level declaration is intentionally allowed through.
The backend later decides whether it is a tail loop (`while`) or must fall back
to guarded inline expansion.

---

## 3. Emitter-side tail-loop recognition and `while` compilation (`BoundedRecursion.hs` + `OpenQASM.hs`)

**Helper extraction (`BoundedRecursion.hs`):** currently provides
`extractTopLevelFunction`, which lets the emitter inspect a top-level
self-recursive declaration as a parameter list plus body. The same module also
contains counted-recursion recognizers for planned `CFor`-based lowering, but
those are not yet wired into `CompilePipeline.hs`.

**Recognition (`isTailLoop` in `QASMAnalysis.hs`):** identifies self-recursive
declarations where every recursive call passes the outer continuation unchanged
— either directly or via η-trivial chains (including Appel's curried-
application intermediates). These are tail loops (Class 2 candidates).

**Class 2 qubit-neutrality is already enforced by `isTailLoop`:** a function
that accumulates qubits across iterations must build a new continuation that
captures newly allocated qubits (e.g. `init_n` builds a growing `Cons` chain).
This modifies the continuation, causing `isTailLoop` to return `False`. Under the
linear-typing assumption, a function that passes `isTailLoop = True` cannot
accumulate qubits. No additional while-loop qubit-neutrality check is required.

`isTailLoop` is a structural check over the emitted CPS shape. It assumes the
well-formed continuation patterns produced by `ToCPS.hs`; it is not a separate
typed proof of qubit-neutrality.

**Class 3 early rejection (`OpenQASM.hs`, using `QASMAnalysis.bodyAllocatesQubits`):** before falling back to guarded
inline expansion, the emitter now rejects the case where a self-recursive
qubit-allocating function is called with no compile-time constant integer
argument. This closes the earlier soundness hole around programs like
`init_n x`, which would otherwise require an unbounded qubit array.

**Emission:** a recognized tail loop emits as
an OpenQASM 3.0 `while` loop. The loop body is the function body; the
recursive call becomes the loop back-edge; the base case becomes the exit
condition.

```openqasm
// Example: rus_loop (repeat-until-success)
while (!m) {
  // loop body
}
```

**Current fallback for non-tail-loop self-recursion:** guarded inline expansion
with a depth limit of 1000 calls. This remains the path for the static bounded
cases that have not yet been lowered to `CFor`. Dynamic qubit-growing cases are
rejected earlier as Class 3 errors. This is transitional — see
[../future/bounded-recursion.md](../future/bounded-recursion.md) for the planned
replacement (static shape inference + `CFor` IR + static list erasure).

---

## Supported recursion patterns

| Pattern | Class | Handling |
|---|---|---|
| Tail loop, qubit-neutral body | 2 (dynamic) | Compiled to `while` |
| Tail loop, qubit-accumulating, static trip count | 1 (static) | Planned: `for` loop via `CFor` IR |
| Tail loop, qubit-accumulating, runtime trip count | 3 (reject) | Compile-time error before inline expansion |
| Non-tail-loop self-recursion, static trip count | 1 (static) | Inline expansion (depth 1000); planned: `CFor` IR |
| Non-tail-loop self-recursion, runtime trip count, qubit-accumulating | 3 (reject) | Compile-time error before inline expansion |
| Non-tail-loop self-recursion, runtime trip count, qubit-neutral | transitional | Guarded inline expansion (depth 1000) |
| Multi-function `CFix` with any recursion | — | Compile-time error |
| Top-level label cycles | — | Compile-time error |

---

## Examples

| Example | Pattern | How compiled |
|---|---|---|
| `rus.funq` | `rus_loop` recurses with unchanged continuation | `while` loop |
| `countdown.funq` | `countdown_zero` recurses on `n-1` | `while` loop |
| `ghz.funq` | `init_n`, `cnot_layer`, `meas_all` recurse over lists | Budget-unrolling (depth 1000) |
| `qft_n.funq` | `init_n`, `meas_all`, `qft_core` recurse over lists | Budget-unrolling |
