# Ohori and Sasano — *Lightweight Fusion by Fixed Point Promotion*

Atsushi Ohori, Isao Sasano, 2007. Status: **initial notes**.

See also: [index.md](index.md), [peyton-jones-specconstr.md](peyton-jones-specconstr.md), [leijen-lorenzen-trmc.md](leijen-lorenzen-trmc.md), [../future/bounded-recursion.md](../future/bounded-recursion.md), [../pipeline.md](../pipeline.md)

---

## Why this paper matters after SpecConstr

The SpecConstr paper was about simplifying the state space of **one** recursive
function when call-site constructor shapes are already known.

This paper attacks a different upstream problem:
- a recursive producer builds a list/tree
- a recursive consumer immediately traverses that result
- the intermediate structure exists only because the two functions are still
  separated

For FunQ, that makes this paper relevant as a possible **recursive
deforestation** step before bounded-recursion recognition. It is not about the
final loop representation; it is about removing an avoidable recursive boundary
so later passes see a simpler worker.

---

## Core claim

The paper proposes a lightweight fusion rule for compositions of the form:

```text
f ∘ (fix g. λx. E)
```

The result is a new recursive function:

```text
fix h. λx. E'
```

where `h` is the fused worker obtained by **promoting** the outer function `f`
through the inner fixed point.

The transformation is driven by two rules:
- **AppDist**: if the outer function is strict, distribute it to every tail
  position of the producer body
- **FixPromote**: after one round of inlining and simplification, replace the
  reappearing `f (g ...)` pattern with a fresh fused worker and tie the knot

The important practical claim is that this is:
- direct: it works on ordinary recursive definitions rather than requiring
  `build`/`fold` encodings first
- general: it applies to user-defined algebraic data types, not just lists
- broad enough for practice: it handles accumulating parameters and
  multi-parameter functions in both curried and uncurried form

---

## The transformation shape

The fusion path is deliberately small:

1. Detect a composition like `f (g x)`.
2. Inline the inner recursive function `g` once.
3. Push `f` into the tail positions of the inlined body with `AppDist`.
4. Inline `f` once and simplify.
5. If the same composition shape reappears recursively, replace it with a new
   fused name `fg` and build `fix fg. ...`.

The paper then packages this inside an inliner:
- maintain an environment of recursive bindings
- memoize whether fusion for each pair `(f, g)` succeeded or failed
- reuse the fused worker when the same pair appears again
- fall back to ordinary inlining when fusion does not fire

That is the main engineering point. This is not presented as a global search
procedure or theorem-driven synthesis pass; it is presented as a tiny extension
to normal inlining.

---

## What it buys

The successful examples are exactly the kinds of pipelines that matter for
compiler normalization:
- `sum (mapsq xs)`
- `sum (from a b)`
- `foldl (...) (from a b)`
- `allTrue (map even xs)`
- tree analogues such as `sumTree (mapsqTree t)`

The fused results remove the intermediate list/tree and expose one recursive
worker directly.

Two practical claims matter:
- on the paper's targeted examples, runtime and heap usage improve because the
  producer result is never allocated as an intermediate structure
- on larger SML benchmarks, enabling the transformation did not introduce
  noticeable runtime overhead when fusion failed to trigger

That second claim is important for FunQ. A conservative normalization pass is
much easier to justify if "no match" is close to "ordinary inlining only".

---

## Limits and non-transferable parts

Several caveats matter immediately for FunQ.

### 1. The proof story is not call-by-value + effects

The paper proves soundness using denotational semantics for a call-by-name
setting and requires the outer function in `AppDist` to be strict.

The authors explicitly call out call-by-value correctness as future work, and
they also note that their implementation does not account for imperative
effects.

That is not a minor footnote for FunQ. Evaluation order matters here because
measurement and classical control are observable, and qubit linearity forbids
duplication or reordering that would be harmless in a pure setting.

### 2. The algorithm is intentionally incomplete

The method is lightweight because it is narrow:
- it treats a pair `(f, g)` as the fusion unit
- it inlines each function only once during a fusion attempt
- it does not fuse repeated self-composition such as `mapsq (mapsq xs)`
- it does not handle mutually recursive producers/consumers

That is acceptable as an optimization pass, but it means the paper is not a
general recursive normalization procedure.

### 3. Visibility matters

The implementation described in the paper only fuses functions whose recursive
bodies are directly available to the inliner. Cross-module/library barriers
shrink the opportunities.

For FunQ this suggests that, if adopted, the pass should happen before later
pipeline stages obscure the original recursive structure.

### 4. This is not a legality proof for bounded recursion

Fusion can simplify a recursive program dramatically, but it does not prove:
- static trip counts
- bounded list sizes
- bounded qubit usage
- legality of OpenQASM lowering

So it cannot replace the existing Class 0 / 1 / 2 / 3 split in the
bounded-recursion plan.

---

## FunQ-specific takeaways

### 1. The best fit is likely before CPS conversion

This paper is phrased over direct-style fixed points, constructor cases, and
tail positions in the source term. FunQ's middle end quickly commits to CPS,
where the same structure is represented much less directly.

So the natural fit is not in the current emitter-side recursion logic. If FunQ
uses this idea at all, the most plausible placement is a small normalization
pass on `LambdaIR` before `ToCPS.hs`, or some similarly early direct-style
stage.

This is an inference from the paper plus the current pipeline, not something
the paper states directly.

### 2. It complements SpecConstr and TRMC rather than replacing either

The three papers now separate cleanly:
- **SpecConstr** simplifies the recursive state of one function based on known
  argument shapes
- **fixed-point promotion** removes a producer/consumer boundary between two
  recursive functions
- **TRMC/TMC** expose residual post-recursive work inside one recursive
  function

That suggests a plausible staging for future work:
- small inlining / constant propagation / specialization
- recursive fusion when a producer feeds a consumer directly
- TRMC-style recognition of residual context
- bounded-recursion classification and lowering

### 3. The safest initial target is pure classical recursive plumbing

Given the call-by-value/effects caveat, the obvious first fragment is:
- first-order functions
- no `init`
- no `meas`
- no gate application
- ordinary lists/tuples/trees carrying classical data only

That would make this technique a Bucket 0 normalization tool first, not a
quantum lowering mechanism.

### 4. It could remove some intermediate recursive aggregates before list erasure

One recurring problem in FunQ is that recursive structure is sometimes only
intermediate plumbing on the way to a simpler traversal. Fixed-point promotion
offers a way to remove such plumbing before static list erasure or loop
recognition.

That matters because a fused worker may be much easier to classify:
- maybe it evaluates away completely
- maybe it becomes a clear counted traversal
- maybe it exposes a simpler state machine for later recognition

### 5. Tempting quantum cases should be treated as future work, not phase one

A composition like a recursive producer of qubits followed immediately by a
recursive consumer is exactly the kind of shape this paper makes one want to
fuse.

But in FunQ those are precisely the cases where:
- allocation order matters
- measurement order matters
- qubit values cannot be duplicated or silently discarded

So the paper is a good source of **structure**, but not yet a correctness
argument for quantum-heavy fusion.

---

## Concrete implementation guidance for FunQ

If FunQ adopts anything from this paper, the conservative version should be:
- run it on direct-style `LambdaIR`, not on the backend emitter
- restrict it to first-order recursive producer/consumer pairs initially
- require visible local definitions and no mutual recursion
- require the outer consumer to be strict in the fused argument
- limit the initial fragment to pure/classical code
- treat failure as "do nothing" and rely on ordinary inlining behavior

The most useful success criterion is not "fuse everything". It is:
- eliminate obviously intermediate recursive aggregates
- simplify recursive state before bounded-recursion classification
- avoid changing effect order or linear-usage obligations

So the main value of this paper for FunQ is as an **upstream normalization
reference**. It suggests a small, deterministic way to simplify recursive
producer/consumer pipelines before the compiler asks whether the remaining
recursion should disappear, become a `for`, become a `while`, or be rejected.
