# Leijen and Lorenzen — *Tail recursion modulo context: An equational approach*

Daan Leijen, Anton Felix Lorenzen, 2025. Status: **initial notes**.

See also: [index.md](index.md), [../future/bounded-recursion.md](../future/bounded-recursion.md), [../passes/recursion.md](../passes/recursion.md)

---

## Why this paper matters now

This is the first paper in the current reading list that is directly about the
remaining recursion problem in FunQ rather than the CPS/SSA backend problem.
It is the right next read before the OCaml TRMC paper because it gives the
general transformation first:
- how to recognize recursive calls that are "almost tail recursive"
- how to turn them into tail-recursive workers by accumulating a context
- how different context representations recover familiar accumulator patterns

For FunQ, that matters because examples like `init_n`, `meas_all`, `cnot_layer`,
and `qft_core` are not hard because they are recursive in the abstract; they are
hard because the recursive calls sit under constructors or other residual
contexts that must be made explicit before we can lower them to finite loops.

---

## Core claim

The paper generalizes tail recursion modulo cons into **tail recursion modulo
context** (TRMC).

The key abstraction is an abstract context type `ctx a` with three operations:
- `ctx E`: turn an evaluation context into a runtime context value
- `app k e`: apply a context to a value
- `k1 • k2`: compose contexts

The whole translation is parameterized only by two laws:
- applying a freshly built context is the same as plugging a hole
- applying a composed context is the same as nested application

Once those laws hold, the paper derives a generic source-to-source
transformation that converts a recursive function into a worker with an extra
context parameter.

This is the important shift for FunQ: the paper is not giving one special
rewrite for `map`; it is giving a reusable recipe for turning many
non-tail-recursive structural functions into tail-recursive workers.

---

## The general algorithm

The paper calculates five transformation equations:
- float `let` bindings outward when they block a recursive call
- float `match` outward in the same way
- A-normalize applications on demand
- when a recursive call appears under an allowed context `E`, turn it into a
  tail call with an extended accumulator/context
- otherwise fall back to ordinary context application

Two points matter most:
- the transformation is **selective**: it only optimizes recursive calls under
  contexts supported by the chosen instantiation
- the `let`/`match` floating means the algorithm is not limited to programs
  already written in A-normal form

The paper proves:
- soundness of the transformed worker
- exhaustiveness in the sense that, when the context condition is unconstrained,
  every recursive call not under a lambda can be exposed and matched

That is a useful reference point for FunQ because the current bounded-recursion
plan needs an explicit recognition story, not just a few ad hoc examples.

---

## Instantiations that matter for FunQ

### 1. Evaluation contexts -> CPS workers

If the allowed context is any evaluation context, TRMC becomes a CPS-style
transformation. This is the most general case, but it usually requires closure
allocation or an equivalent representation.

This matters conceptually because FunQ already has CPS in the middle end. The
paper reinforces that "recursive call plus residual work" can be represented as
explicit carried context rather than hidden stack structure.

### 2. Defunctionalized evaluation contexts -> explicit accumulators

Defunctionalizing the finite set of possible residual contexts produces an
accumulator datatype. For `map`, this yields the usual accumulator/reverse
program; more generally, it turns context shapes into explicit data.

This is probably the closest reusable idea for FunQ. A bounded-recursion pass
does not want first-class higher-order contexts in the emitted program, but it
may want to recognize a finite family of residual shapes and turn them into
explicit loop-carried state.

### 3. Associative / monoid / semiring contexts -> specialized accumulators

The paper shows that some recursive patterns do not need general contexts at
all:
- addition contexts give the textbook `length` accumulator
- right-biased append contexts give the textbook `reverse` accumulator
- semiring contexts turn expressions like `x + 31 * hash(xs)` into a worker
  with two accumulators

This is useful for FunQ's classical fragment. Some recursive helpers may be
eliminable by a simpler algebraic accumulator transformation instead of general
list-shape machinery.

### 4. Constructor contexts -> TRMc / list-building

The constructor-context instance is the direct generalization of TRMc. It is the
paper's main practical case and the one most obviously related to recursive list
construction.

For FunQ, this is the closest match to list-producing structural recursion like
`init_n` or `meas_all`: the recursive call is followed by constructor work that
needs to become explicit state.

---

## Constructor contexts and the runtime story

The paper gives two levels of account for constructor contexts:
- a clean abstract account via Minamide's hole calculus
- a concrete in-place-update account via Perceus heap semantics and reference
  counts

The important result is not just that constructor contexts are expressive; it is
that the generated contexts are used **linearly**, which makes constant-time
composition and application sound.

This runtime story does **not** transfer directly to FunQ:
- OpenQASM has no heap data structures or first-class constructor contexts
- FunQ cannot rely on runtime context mutation to implement recursion

What does transfer is the static insight:
- structural recursion under constructors can be decomposed into
  "tail-recursive worker + explicit carried context"
- once that context is explicit, later lowering can replace it with a counted
  loop plus fixed aggregate updates instead of recursive emission

So the constructor-context part is mainly a normalization reference for FunQ,
not a target runtime design.

---

## Limits and non-transferable parts

Several constraints matter when mapping this paper to FunQ.

### 1. TRMC removes stack recursion, not output-size constraints

Turning `init_n` into a tail-recursive worker does not by itself solve FunQ's
real backend problem: the total qubit count still has to be statically known.

So TRMC is not a substitute for:
- static shape inference
- `CFor` lowering
- rejection of dynamic qubit-allocating recursion

### 2. The transformation only targets certain recursive positions

Recursive calls under lambdas are intentionally not handled. The algorithm also
optimizes the first recursive calls in evaluation order; later recursive calls
may require composition with a more general context representation.

This is relevant for nested traversals and more complex tree/list programs.

### 3. Effectful code constrains motion

The paper is careful about evaluation order. Some apparently simple rewrites only
go through once effectful computations are evaluated before context creation.

That matters for FunQ whenever classical control or measurement results are
involved: any normalization pass must preserve the exact sequencing of classical
effects and quantum operations.

### 4. Multiple-hole contexts are outside the main technique

Single-hole context accumulation handles list-building naturally, but not shapes
that need one recursive result in multiple places. The paper discusses this as a
real limitation.

That makes it a poor direct fit for duplication-shaped recursion, though linear
quantum data already rules out many of those cases in FunQ.

---

## FunQ-specific takeaways

### 1. Use TRMC as a recognition recipe, not as the final representation

The paper gives a disciplined way to expose residual context around recursive
calls. For FunQ, that is best treated as an upstream normalization step before
loop lowering, not as the backend IR itself.

### 2. Defunctionalized contexts are the most plausible import

FunQ should not import Koka-style first-class constructor contexts, but it may
benefit from the paper's defunctionalized reading: turn a finite set of residual
recursive contexts into explicit loop-carried state.

### 3. Constructor-context recursion is exactly the right lens for list builders

`init_n` and similar functions are naturally read as "recursive call plus one
more constructor layer". The paper clarifies how to separate those layers from
the control recursion.

### 4. Algebraic contexts may simplify some classical helpers

The semiring/monoid instances suggest a second, cheaper path for classical-only
helpers: when the residual work is just arithmetic or append-like accumulation,
FunQ may be able to remove recursion without full structural shape analysis.

### 5. The paper sharpens the Class 1 / Class 2 / Class 3 split

TRMC can make recursion tail-recursive, but it does not change whether the
program is:
- statically bounded and lowerable to `for`
- dynamically qubit-neutral and lowerable to `while`
- dynamically qubit-allocating and therefore invalid for OpenQASM

That supports the current FunQ plan rather than replacing it.

---

## Concrete implementation guidance for FunQ

Near-term lessons for the bounded-recursion work:
- keep `CFor` / early rejection as the target plan; do not treat TRMC as an
  alternative backend
- use the paper as the main reference for **recognizing** structural recursion
  under constructors or algebraic contexts
- prefer defunctionalized or purely static context representations over
  higher-order contexts
- after context extraction, lower the worker's carried state into explicit loop
  variables / fixed aggregates, then erase lists before emission

In practical terms, the paper suggests the right decomposition for examples such
as `meas_all`, `cnot_layer`, and `qft_core`:
- first expose the residual list/tuple-building or arithmetic context around the
  recursive call
- then identify the bounded iteration space
- then lower to `CFor` or compile-time evaluation

This is the best theoretical reference so far for the "how do we turn
structural recursion into explicit loop state?" part of the FunQ roadmap.
