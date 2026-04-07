# Chapter 3 — Semantics of the CPS

Pages 23–36. Status: **complete**.

---

## Overview

Appel gives a denotational semantics for the CPS language, written in Standard
ML. The point is not just formal elegance: it makes the later CPS
transformations checkable against one shared meaning. Every variant of the CPS
language uses this same semantic core, regardless of which extra syntactic
restrictions (scope discipline, closure-converted form, etc.) have already been
imposed.

The top-level evaluator has the shape:

```sml
eval : var list * cexp -> dvalue list -> store -> answer
```

So a CPS compilation unit is interpreted relative to:
- a list of external variables,
- concrete denotable values for those variables, and
- a store.

This is Appel's semantic account of linkage: externally supplied functions and
runtime services can simply be passed in as denotable values.

---

## Semantic parameters

The semantics is deliberately parameterized over machine-level choices:
- `minint`, `maxint`, `minreal`, `maxreal` for finite arithmetic
- `string2real` for parsing real literals
- `loc` and `nextloc` for store allocation
- `arbitrarily` for implementation-dependent behavior
- an abstract `answer` type for the whole program result

This keeps the CPS semantics independent of any one target machine while still
being concrete enough to talk about overflow, allocation, mutation, and control.

---

## Denotable values and the store

The denotable-value domain includes:
- `RECORD of dvalue list * int`
- `INT`, `REAL`, `STRING`
- `ARRAY`, `UARRAY`, `BYTEARRAY`
- `FUNC of dvalue list -> store -> answer`

Two structural choices matter most:

### 1. Records are pure values

Records are *not* stored in the mutable store. A record value directly contains
its fields plus an integer offset. This means records are immutable and can be
freely copied or shared semantically.

### 2. Mutable objects live in the store

Arrays, unboxed arrays, byte arrays, and the current exception handler are
represented through locations in the store. Mutation is therefore modeled by
producing a new store.

This separation is important for later chapters:
- pure record manipulation can stay in ordinary CPS dataflow,
- mutation is isolated to explicit primops and store threading,
- record offsets already justify `OFFSET` and the nested access paths used later
  by closure conversion and spilling.

---

## Equality is intentionally weak for pure heap values

The `eq` function is exact for:
- integers,
- arrays / unboxed arrays / byte arrays, by location identity.

But it is only *conservative* for:
- records,
- strings,
- reals.

If two immutable values are unequal, `eq` must return `false`. If they are
equal, `eq` may return either `true` or `false`. This models the fact that an
implementation might hash-cons equal values to the same address, or copy them so
that equal values live at different addresses.

This is a subtle but important semantic contract:
- pointer identity is meaningful for mutable objects,
- pointer identity is not semantically reliable for immutable records/strings,
- functions are not comparable at all.

The `boxed` primop exposes a related implementation assumption: whether a value
is represented as a pointer or as an unboxed integer.

---

## Primitive operators choose continuations

`evalprim` takes:
- a primitive operator,
- a list of denotable arguments,
- a list of continuation functions.

Non-branching primops compute result values and pass them to one continuation.
Branching primops choose one continuation from the list. Overflow and division
by zero raise via the current handler continuation stored at `handler_ref`.

This makes a core CPS property semantic, not merely syntactic:

**every CPS operator continues with exactly one continuation.**

For `RECORD`, `SELECT`, and `OFFSET`, the continuation is the explicit subterm.
For `SWITCH` and branching `PRIMOP`, the semantics selects exactly one branch.
For `APP`, the continuation is whatever control flow is embedded in the called
function body.

---

## Environments, lexical scope, and function meaning

The semantic function for values,

```sml
V : env -> value -> dvalue
```

looks up `VAR`/`LABEL` in the environment and interprets literals directly.

The semantic function for expressions,

```sml
E : cexp -> env -> store -> answer
```

gives each CPS constructor its meaning:
- `SELECT` fetches a record field and binds it
- `OFFSET` changes only the record pointer offset
- `RECORD` builds a new immutable record after evaluating access paths with `F`
- `SWITCH` chooses the indexed continuation
- `PRIMOP` delegates to `evalprim`
- `APP` applies a denotable function to evaluated arguments

`APP` does **not** pass a dynamic environment. The called `FUNC` already carries
the environment it needs. So the semantics is explicitly lexical, not dynamic.

---

## `FIX` explains mutual lexical recursion

The `FIX` case is the semantic heart of the chapter. Appel defines mutually
recursive helper functions `g` and `h` so that:
- `g env` extends the current environment with all function names in the `FIX`
- each function body, when called, reuses `g` to rebind the whole recursive
  group before binding its formal parameters

This is exactly the semantic mechanism needed for nested mutually recursive CPS
functions before closure conversion. Chapter 10 will later remove nontrivial free
variables operationally, but Chapter 3 already explains what those nested
functions *mean*.

---

## Access paths already model interior pointers

The helper function `F` evaluates access paths:
- `OFFp 0` leaves a value unchanged
- `OFFp j` shifts a record pointer by `j`
- `SELp(j, p)` selects field `j` and then continues following `p`

This is the semantic justification for later implementation techniques where a
value may point into the middle of a shared closure or spill record rather than
to its first field.

That is directly relevant to FunQ's current CPS:
- `COffset` has a clean semantic reading here
- shared closure layouts are not a special-case hack
- Chapter 11's spill-record access paths are already anticipated

---

## FunQ-specific takeaways

### 1. Single-successor control is built into the meaning

The semantics makes explicit that a CPS node does not "return" in the direct
style sense. It transfers control to exactly one continuation. This is the right
foundation for treating join continuations as backend control-flow nodes.

### 2. Closure-sharing machinery already has semantic support

Because records may carry offsets and access paths may traverse interior fields,
the shared-closure strategy used later in Appel and in FunQ is semantically
natural.

### 3. Pure aggregates and mutable state should stay separate

Appel keeps immutable records out of the store and restricts mutation to arrays
and refs. FunQ should be stricter still: qubits must not be duplicated into heap
records at all, so the "mutable state lives elsewhere" discipline is even more
important.

### 4. This chapter is a proof template, not a finished quantum semantics

Appel's semantics is enough to reason about classical control flow, lexical scope,
and CPS rewrites. A full FunQ semantics would need extra structure for linear
qubits and consuming measurement, but it should preserve this same overall
shape: explicit environments, explicit control transfer, and explicit state
threading where mutation exists.

---

## Sections less directly relevant to the current FunQ compiler

- the detailed ML exception-handler machinery via `handler_ref`
- mutable array and byte-array primops
- the precise runtime tagging assumptions behind `boxed` and representation
  tests

These are still useful background if FunQ ever grows a lower-level runtime or a
QIR-oriented memory model, but they are not the immediate payoff of the chapter.
