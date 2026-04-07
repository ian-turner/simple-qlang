# Chapter 11 — Register Spilling

Pages 125–132. Status: **complete**.

---

## Overview

After closure conversion, every CPS variable corresponds directly to a machine
register. But inside a function body, the number of simultaneously live
variables can exceed N (the machine's register count). The spill phase rewrites
the CPS so that every subexpression has fewer than N free variables.

**Liveness = free variables**: a variable is live at a point if it appears free
in the continuation expression from that point onward. The fv() function from
Chapter 2 computes exactly this — no separate dataflow analysis needed.

The spill phase runs after closure conversion and sees a flat CPS expression
(no nested FIX operators). It is applied independently to each top-level
function body.

---

## The spilling mechanism

When the live-variable count is about to exceed N, pack the current live set
into a heap RECORD — the *spill record*. Now only one register is needed for
the pointer to that record. Variables are fetched with SELECT as needed.

Key insight: **duplicate variables** are those held in both a register and the
spill record. The register copy can be discarded at any time to free a slot;
if the variable is needed again it can be re-fetched from the record.

---

## Worked example: N = 5 (pp. 126–127)

A chain of SELECT operations accumulates free variables one per step. Once 4
are live (approaching the limit), the algorithm creates a spill record before
the next operator that would push past N.

**Subtle point**: variables `a` and `b` are *not* written to the first spill
record even though they are live at that point. The algorithm observes that
keeping them in registers does not increase the count past N (given the spill
record pointer already counts as one register slot), so it avoids the
redundant store.

The example requires two spill records. The second uses the `SELp` access
path notation to reference fields of the first record rather than copying each
value individually:

```
RECORD([(VAR b, OFFp 0), (VAR e, OFFp 0), ...,
        (VAR r, SELp(2, OFFp 0)), (VAR r, SELp(3, OFFp 0)), ...], s, ...)
```

This is the only place in the compiler where access paths deeper than
`OFFp 0` appear. It allows creating a large spill record without exceeding N
free variables in the RECORD expression itself.

---

## §11.1 Rearranging the expression (p. 128)

In the example, simply reordering the SELECT operators eliminates all spilling
(shown on p. 128). Optimal reordering is NP-complete (equivalent to optimal
register allocation on directed acyclic graphs). Appel's empirical observation:
with N = 22 registers, spills occur roughly once per several thousand lines of
compiled code. Fancy reordering heuristics are not worth implementing.

**FunQ note**: similarly, FunQ programs are unlikely to require spilling for
classical variables, but the static qubit-width check (below) matters more.

---

## §11.2 The spilling algorithm (pp. 128–132)

The spill function F is applied to each function body. It is one-pass
(top-down, recursive). Its signature:

```
F(R, U, D, S_c, S_v, e) → e'
```

### Context parameters

| Parameter | Meaning |
|---|---|
| R | Variable just bound by the immediately previous operator (cardinality 0 or 1) |
| U | Uniquely bound live variables **not** in the spill record; must stay in registers |
| D | Duplicate variables — in both the spill record and a register; register copy is expendable |
| S_c | Set of variables stored in the current spill record (∅ if no spill record exists) |
| S_v | Variable naming the current spill record |

For each expression e, let:
- A = set of arguments (operands of the root operator of e)
- W = set of variables bound by the root operator
- C = set of continuation expressions of the root operator
- V_before = fv(e) — free variables before the root operator executes
- V_after = ⋃_{c ∈ C} fv(c) — free variables after the root operator executes

### Step 1: determine if spill record is still useful

Let S_before = {S_v} if a spill record exists, else ∅.

If S_c ∩ V_after = ∅, the spill record contains no variables needed after this
operator. Let S_c^after = ∅ (the record can be discarded). Otherwise
S_c^after = S_c, S_v^after = S_v.

### Step 2: trim duplicates

Available register slots for duplicate variables:

```
N_dup = N − |S_before| − |(U ∩ V_before) ∪ R|
```

If N_dup < |D|, discard excess duplicates, keeping those whose first use is
most deeply nested (i.e., most distantly needed — they can be re-fetched
from the record when required). Start by discarding duplicates not in V_before
at all (never needed again).

### Step 3: decide whether to spill

Two independent conditions trigger a new spill:

1. **Arguments trigger**: `|A ∪ (U ∩ V_after)| > N − |S_v^after|`
   Cannot simultaneously hold all arguments to the current operator plus all
   irreplaceable live values.

2. **Result trigger**: `|W ∪ (U ∩ V_after)| > N − |S_v^after|`
   Cannot hold the operator's results plus all irreplaceable live values needed
   afterward.

### Step 4a: if spill needed

Create a new spill record containing V_before:

```
F(R, U, D, S_c, S_v, e) =
  RECORD(get(V_before), S_v', F(∅, ∅, D'', V_before, S_v', e))
```

where `get(V_before)` produces record fields:
- For variables in D or U: `(v, OFFp 0)` — direct value
- For variables in S_c: `(S_v, SELp(i, OFFp 0))` — interior pointer into old record

The recursive call has U = ∅, so it is guaranteed not to spill again at the
root of e. D'' = (U ∪ D) ∩ V_before — the subset of new contents currently in
registers.

### Step 4b: if no spill needed

Some operands A may need to be fetched from the current spill record. Let
F = A − (U ∪ D') be the set that must be fetched.

- |F| = 0: emit the root operator directly, recurse on continuations
- |F| = 1: emit `SELECT(i, S_v, v', ...)` to fetch the variable; this is also
  the last fetch from this record (S_c^after = ∅)
- |F| > 1: emit `SELECT(i, S_v, v', ...)` for the first fetch, keeping the
  spill record live (S_c unchanged)

### Complexity note

The algorithm is one-pass. The "one spill per operator" guarantee holds because
the recursive call after creating a spill record passes U = ∅. Spills at
structure-creation sites (RECORD) are most common but structures are static
objects (created once per function call), so the runtime cost is negligible.
Appel's recommendation: check each function body first; only apply F if
spilling is actually needed.

---

## FunQ adaptation

### Classical variables

Appel's algorithm applies directly to classical (Bool, Int) variables in FunQ.
The spill record is a heap-allocated RECORD; variables are fetched with SELECT.

### Qubits — no heap spilling

Qubits cannot be stored in heap records (no-cloning theorem). Therefore:

**Qubit spilling is not implemented.** Instead, the compiler statically
computes the maximum qubit width (maximum number of simultaneously live qubits)
across each function body using the same free-variable counting logic.

- If the maximum qubit width ≤ hardware qubit count → compilation succeeds
- If the maximum qubit width > hardware qubit count → compile-time error

This is equivalent to Appel's N-register bound, but without a spill fallback.

**Practical note**: well-structured quantum programs tend to measure qubits
and reuse them, keeping qubit width low. The static check is primarily a
sanity/capacity check for the target hardware, not a bottleneck in practice.

The same fv() free-variable analysis used for classical spilling provides
qubit width: simply compute `max over all subexpressions of |{q ∈ fv(e) | q is a Qubit}|`.

---

## Sections not relevant to FunQ

- Access-path elaboration for very large spill records (§11.2, the SELp/OFFp
  bookkeeping) — only needed if a single function has an extreme number of
  live variables; unlikely in practice
- Rearranging expressions for optimal register allocation (§11.1) — NP-complete
  and empirically unnecessary; not implemented
