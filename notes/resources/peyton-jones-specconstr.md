# Peyton Jones — *Call-pattern Specialisation for Haskell Programs*

Simon Peyton Jones, 2007. Status: **initial notes**.

See also: [index.md](index.md), [leijen-lorenzen-trmc.md](leijen-lorenzen-trmc.md), [allain-bour-clement-pottier-scherer-tmc-ocaml.md](allain-bour-clement-pottier-scherer-tmc-ocaml.md), [../future/bounded-recursion.md](../future/bounded-recursion.md), [../passes/static-shape.md](../passes/static-shape.md)

---

## Why this paper matters now

The TRMC and OCaml TMC papers explain how to expose residual work around
recursive calls so that structural recursion can become explicit loop-carried
state. This paper is useful as the next read because it attacks a different,
upstream problem:
- when recursive calls are made with arguments whose constructor shape is
  already known
- when the callee immediately pattern-matches those arguments again
- when specializing on those shapes reveals a cheaper loop

For FunQ, that makes this paper relevant less as a direct backend design and
more as a guide for **shape-driven recursive specialization** before bounded
recursion lowering. It is especially suggestive for recursive helpers whose
work is obscured by wrappers like `Left`/`Right`, tuples, or list constructors.

---

## Core claim

The paper introduces **SpecConstr**: specialize recursive functions according to
the constructor shapes of their arguments at recursive call sites.

The key idea is simple:
- inspect recursive calls in a function body
- record call patterns where some arguments are known constructor forms
- make a specialized copy of the function for those patterns
- rely on ordinary simplification afterward to eliminate the now-redundant
  case analysis and intermediate allocation

This is not full partial evaluation. The specialization key is the **shape** of
arguments, not their full value. The paper's examples show two main wins:
- remove repeated pattern-matching when a recursive call already proves the
  constructor form of an argument
- remove allocation/reboxing when a loop repeatedly constructs a wrapper only
  for the callee to deconstruct it immediately

---

## The transformation in one pass

The implementation is intentionally modular:
- identify call patterns worth specializing
- clone the function body, instantiated with pattern variables for the
  specialized shape
- emit rewrite rules so later simplification rewrites matching calls to the
  specialized copy and ties the recursive knot

The paper's important engineering claim is that the middle step stays very
small. The specialized function is mostly just a fresh copy of the original
body with the shaped arguments substituted in; the ordinary simplifier removes
the dead cases and redundant constructors.

For FunQ this matters because it suggests a clean staging:
- use one pass to expose useful recursive call patterns
- let existing simplification/constant propagation clean up the specialized IR
- only then hand the result to bounded-recursion recognition or loop lowering

---

## Heuristics that matter

The initial specialization heuristics are deliberately narrow:
- only functions with a statically visible definition and explicit arity
- only recursive functions
- only recursive calls inside the function body itself
- only saturated calls
- only calls with at least one constructor argument
- only when that argument is actually scrutinized in the callee body

Two aspects are especially relevant for FunQ.

### 1. Specialize only on scrutinized arguments

The paper does not specialize on every structured argument. It specializes only
when the callee later inspects that structure. This keeps the transformation
targeted and avoids pointless code duplication.

That is a good fit for FunQ. If a recursive helper threads a tuple/list value
through unchanged, there is little reason to split it apart. If it repeatedly
matches on the head/tail or on a sum-like tag, specialization is much more
interesting.

### 2. Shapes can be nested and discovered through known bindings

The paper refines its initial heuristic so that specialization can use:
- constructor information learned from surrounding case branches
- nested constructor structure, not only the outermost constructor
- iterative fixed-point discovery, where one specialized copy exposes new call
  patterns for another

This is one of the strongest takeaways for FunQ. Many recursive programs do not
become simple because one parameter is merely "a list"; they become simple
because the compiler knows the recursive state is specifically "non-empty list"
or "left state carrying the remaining prefix" or "pair whose first field is the
active counter".

---

## Performance story

The paper's benchmark result is strong enough to matter:
- about 10% geometric-mean runtime improvement on the `nofib` suite
- dramatic wins on some stream-fusion kernels, including multi-x improvements
- a modest code-size increase of a few percent

The practical lesson is that specialization is worthwhile when it exposes an
inner loop that other optimizations already want, but is currently hidden behind
constructor churn.

This maps cleanly onto the FunQ roadmap. A recursive helper may already be
"really" a finite traversal, but that fact can be obscured by ADT plumbing in
the current IR.

---

## Limits and non-transferable parts

Several constraints matter when mapping this paper to FunQ.

### 1. SpecConstr is a performance optimization, not a legality proof

The paper helps expose cheaper loops, but it does not prove that a recursive
program has a statically bounded result shape or a statically bounded qubit
footprint.

So it cannot replace:
- static shape inference
- Class 3 rejection for dynamic qubit-growing recursion
- `CFor` lowering

### 2. Reboxing is a real hazard

The paper's main negative result is the **reboxing problem**: specializing on a
destructured argument may save allocation at the recursive call but force the
compiler to rebuild the same structure elsewhere.

This is directly relevant to FunQ because record/tuple flattening already lives
in the pipeline. A shape-specialization pass that eagerly explodes tuples or
sum-like encodings could accidentally fight later passes or duplicate aggregate
reconstruction work.

### 3. The implementation depends on downstream simplification

SpecConstr works because GHC already has an effective simplifier, inliner, and
rewrite-rule mechanism around Core. The paper's elegance partly comes from that
context.

For FunQ, the transferable idea is the specialization criterion, not the exact
mechanism of RULE-based rewrites.

### 4. The paper is not a direct recipe for higher-order or quantum effects

The paper briefly discusses specializing on function arguments and join points
as future work, but its mature implementation focus is still first-order
constructor shapes on recursive loops. It does not answer the effect-order and
linearity questions that matter for measurement-heavy or qubit-carrying FunQ
programs.

---

## FunQ-specific takeaways

### 1. Shape-driven specialization belongs before bounded-recursion lowering

This paper fits best as an **upstream normalization/partial-evaluation aid**.
Before trying to lower a recursive function to `CFor` or classify it as a
dynamic `while`, the compiler may want to specialize away wrapper structure that
is already known at recursive call sites.

### 2. It complements TRMC rather than competing with it

TRMC/TMC explain how to make residual post-recursive work explicit. SpecConstr
explains how to remove redundant shape plumbing around the recursive call
itself. Those are different jobs and can compose well:
- SpecConstr can simplify the recursive state space
- TRMC-style recognition can then expose residual carried context
- bounded-recursion lowering can finally recover explicit loop structure

### 3. Stream-fusion-style state splitting is a good analogy for FunQ

The paper's `Left`/`Right` example is especially relevant. It shows that a
single recursive state machine can often be specialized into several simpler
recursive workers, each corresponding to one known constructor state.

That suggests a useful lens for FunQ traversals over lists and tuples: some of
the remaining recursion work may be easier after splitting by known state shape
rather than handling every recursive state in one generic worker.

### 4. Reboxing risk argues for close coordination with flattening passes

If FunQ grows shape-based specialization, it should be coordinated with:
- interface record flattening
- local record flattening
- any future static list erasure pass

Otherwise the compiler may repeatedly split and rebuild the same aggregates.

### 5. The paper sharpens Bucket 0 in the bounded-recursion plan

The current roadmap already distinguishes "static classical recursion -> evaluate
away" from loop lowering. This paper gives a concrete technique for part of that
bucket: specialize recursive functions by constructor shape so that ordinary
simplification can collapse redundant matches and wrapper allocations.

That is especially plausible for classical recursive helpers that compute
indices, counters, or bounded traversal state.

---

## Concrete implementation guidance for FunQ

If FunQ adopts anything from this paper, the conservative version should be:
- restrict it to first-order self-recursive helpers initially
- specialize only on arguments whose structure is actually inspected
- treat lists, tuples, booleans, and sum-like encodings as the initial shape
  language
- run it after basic inlining/constant propagation but before bounded-recursion
  lowering
- iterate to a fixed point within a recursive group, because one
  specialization may expose another
- add a guard against reboxing-heavy cases, especially where a destructured
  aggregate is also passed through unchanged elsewhere

Most importantly, FunQ should treat SpecConstr as a way to **simplify recursive
state before legality classification**, not as a substitute for proving static
bounds. A specialized recursive function may be easier to lower, but it still
must end up in one of the existing buckets:
- compile-time evaluation
- statically bounded `for`
- dynamic qubit-neutral `while`
- early rejection
