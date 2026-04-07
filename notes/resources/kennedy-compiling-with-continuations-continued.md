# Kennedy — *Compiling with Continuations, Continued*

Andrew Kennedy, 2007. Status: **initial notes**.

See also: [index.md](index.md), [kelsey-ssa-cps.md](kelsey-ssa-cps.md), [maurer-compiling-without-continuations.md](maurer-compiling-without-continuations.md), [../future/backend-refactor.md](../future/backend-refactor.md), [../passes/openqasm-emission.md](../passes/openqasm-emission.md)

---

## Why this paper matters after Kelsey and Maurer

Kelsey is the clearest argument that many CPS continuations already correspond
to SSA/basic-block structure. Maurer argues that join points should be treated
as a distinct control construct instead of being blurred with ordinary
functions. Kennedy is useful as the next step because it defends a more
conservative engineering choice: keep CPS as the main optimization IR, make
continuations explicit and second-class, and improve code generation and
optimization around that representation rather than switching to ANF or a new
IR too early.

For FunQ, that is directly relevant to the current backend problem:
- `src/ToCPS.hs` already generates named local continuations for `LSwitch`
- `src/OpenQASM.hs` still mostly interprets CPS recursively and repairs branch
  joins with suffix hoisting
- the immediate opportunity is to treat those joins as explicit local control
  structure, not to redesign the whole middle end

---

## Core claim

Kennedy argues that a CPS IR with **named, mandatory, second-class
continuations** remains a very practical optimization language even when the
source language has no `call/cc`-style first-class control.

The important representation choices are:
- every control point is named
- local continuations are distinct from ordinary functions
- continuation applications compile as jumps/basic-block transfers
- recursive continuations can represent loops directly
- ordinary functions remain the entities that may need closure/function-call
  treatment

This is a stronger defense of CPS than "CPS and SSA are related". The paper's
message is that explicit continuations are not just a semantic device; they are
useful compiler structure.

---

## What CPS buys over ANF and monadic IRs

Kennedy's central practical argument is about **simplification after inlining**.
In ANF or monadic IRs, inlining often breaks the normal-form discipline, so the
compiler must re-normalize terms with commuting conversions or introduce a
synthetic join point to share the continuation of a branch. In CPS, that join
point is already present as the named continuation argument, so inlining is
much closer to ordinary substitution.

Two points transfer directly to FunQ:
- explicit join points should exist *before* backend lowering, not be invented
  later as a repair step
- once joins are explicit, their locality is visible in the syntax and survives
  transformations better than in ANF-style encodings

This is one of the cleanest arguments so far against relying on emitter-side
shared-suffix recovery as the primary join mechanism.

---

## Continuations as local control nodes

The paper makes a syntactic distinction between:
- functions, which may need closure allocation or ordinary function calling
- local continuations, which can be compiled inline and jumped to directly

That distinction matters because join points and loops are not merely "small
functions". They are control-flow nodes.

For FunQ, this supports an explicit backend rule:
- branch-local continuations created during CPS conversion should be treated as
  backend-local blocks/joins when their uses stay local and tail-positioned
- they should not be forced through the same path as ordinary callable code

This aligns with the existing `LSwitch` join-continuation shape in the CPS IR.

---

## Contification

The paper's most concrete backend contribution is **contification**:
transforming a function into a local continuation when it always returns to the
same place.

For non-recursive functions, the criterion is intentionally simple:
- every use of the function must pass the same continuation argument
- the transformation replaces the function with a local continuation
- the common continuation is substituted into the body
- the new continuation is moved inward to the smallest scope containing all
  call sites

For recursive groups, Kennedy generalizes this to a strongly connected set of
functions:
- calls within the group may tail-call one another
- every call from outside the group must use the same return continuation
  (and the same handler continuation in the exception-setting of the paper)
- such a group can be rewritten into local recursive continuations

The important practical point is that Kennedy presents this as **incremental
rewriting**, not only as a whole-program graph analysis. He then relates the
rewrites back to dominator-based contification and argues that exhaustive
rewriting reaches the same result.

For FunQ this is useful in two ways:
- as a classification criterion for which local CPS bindings are really joins
  or loops
- as evidence that a lightweight, structural analysis may be enough for an
  initial backend improvement

---

## Secondary ideas

Two other parts of the paper are valuable, but are less immediate for FunQ:

### 1. Double-barrelled CPS for exceptions

Kennedy models exceptions by threading both a handler continuation and a return
continuation. FunQ does not currently have this problem, so the mechanism does
not transfer directly. The useful residue is only the general idea that
distinct control paths can still be handled uniformly in CPS.

### 2. Graph-based CPS representation

The paper argues that shrinking reductions on CPS can be implemented very
efficiently with a graph representation plus union-find for binder access. That
is interesting if FunQ later grows a larger simplifier pipeline, but it is not
the immediate backend bottleneck.

---

## Limits and non-transferable parts

Not everything here should drive the next FunQ change:
- the paper is primarily a defense of CPS against ANF/monads; FunQ is already
  committed to CPS, so that comparison is background rather than a new design
  decision
- the exception-handling material is not directly relevant to the current
  OpenQASM backend work
- the graph-representation discussion is more about optimizer implementation
  cost than about emitter semantics

So the direct import is narrower:
- explicit local continuations are useful compiler structure
- joins and loops should stay second-class and local when possible
- contification can be phrased with simple structural conditions

---

## FunQ-specific takeaways

### 1. Staying in CPS is a defensible staged plan

This paper is the strongest support so far for keeping CPS as FunQ's main
middle-end language while improving the backend's treatment of continuations.
There is no need to switch representations just to get better join handling.

### 2. Explicit joins should replace suffix-hoisting as the primary path

Kennedy's argument about named continuations makes the current emitter design
pressure clear: branch-local joins should be emitted as one shared continuation
body with explicit branch exits into it, not primarily recovered by comparing
already-emitted suffixes.

### 3. Join classification can be structural

The paper suggests a simple first cut for classifying backend-local joins:
- local scope
- saturated use only
- tail-position calls only
- common return continuation for all non-local entries
- no escape into value position

That is compatible with an emitter-side analysis over `CFix`-bound labels.

### 4. Some recursive helpers may really be local loops

Kennedy's recursive contification story strengthens the case that certain
recursive local helpers should stay local control flow rather than be treated as
ordinary callable procedures, provided their call structure satisfies the same
common-return criterion.

### 5. This complements, rather than replaces, Kelsey and Maurer

The three papers now line up cleanly:
- Kelsey: CPS joins correspond naturally to SSA/basic blocks
- Maurer: joins should be explicit and second-class
- Kennedy: CPS itself is a practical place to preserve, optimize, and infer
  that join structure

---

## Concrete implementation guidance for FunQ

Near-term backend direction suggested by this paper:
- keep the current CPS IR as the canonical middle-end representation
- add explicit emitter handling for branch-local join continuations from
  `LSwitch`
- classify local join/loop labels structurally instead of compiling every label
  through a uniform callable path
- preserve the current fallback emitter path for unsupported shapes

Possible next-step extension after explicit `LSwitch` joins land:
- add a lightweight late-middle-end or emitter-side contification pass for
  recursive/local helpers that always return to the same continuation

The main practical lesson is straightforward: **FunQ does not need less CPS; it
needs to use the continuation structure it already has more directly.**
