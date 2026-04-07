# Cong et al. — *Compiling with Continuations, or without? Whatever.*

Youyou Cong, Leo Osvald, Grégory Essertel, Tiark Rompf, 2019. Status: **initial notes**.

See also: [index.md](index.md), [kelsey-ssa-cps.md](kelsey-ssa-cps.md), [maurer-compiling-without-continuations.md](maurer-compiling-without-continuations.md), [kennedy-compiling-with-continuations-continued.md](kennedy-compiling-with-continuations-continued.md), [../future/backend-refactor.md](../future/backend-refactor.md)

---

## Why this paper matters after Kelsey, Maurer, and Kennedy

Kelsey, Maurer, and Kennedy already give most of the direct technical guidance
for FunQ's backend work:
- Kelsey: CPS joins already look like SSA/basic blocks
- Maurer: joins should be explicit and second-class
- Kennedy: CPS is still a good optimization IR if continuations are named and
  restricted

Cong et al. matter as the next paper because they reframe the argument at the
design level. Their main point is that "CPS or not CPS" is the wrong question;
the real question is **which continuations should be explicit at which stage of
the compiler**.

For FunQ, that is useful mostly as **scope control**:
- the current backend problem is not "replace CPS"
- it is not even necessarily "invent a new global IR"
- it is "make the right local control structure explicit enough to compile
  cleanly"

That makes this paper a good check against over-designing the emitter refactor.

---

## Core claim

The paper proposes a mixed IR, `λ1/2_⊥`, with three key ingredients:
- a control operator `C` for selectively exposing continuations
- a type distinction between value types and `⊥` to track non-returning terms
- a 1st-class / 2nd-class distinction to track whether values may escape

The headline message is not "use this exact calculus". It is:
- optimize in direct style when that is convenient
- expose continuations only where local control structure matters
- perform CPS translation later, selectively or fully, when lower-level control
  flow work benefits from it

Their slogan is effectively: **as much or as little CPS as you want, when you
want it**.

---

## The critique of Maurer is about rewrite control

The most concrete technical disagreement with Maurer et al. is not about
whether join points are valuable. It is about how much freedom the optimizer has
when moving contexts across join bindings.

Maurer's `jfloat` plus `abort` sequence can create a copy-and-drop pattern:
- first push an outer context into both the join body and the surrounding term
- then immediately discard that context around jumps

Cong et al. argue this is formally awkward and operationally redundant. Their
`C` operator supports a `cfloat` rewrite that pushes the surrounding context only
into the captured continuation, not blindly into the whole body. The practical
point is:
- join-preserving rewrites become more direct
- the optimizer gets finer control over where context is injected
- the formal rewrite story lines up better with what an implementation wants to
  do anyway

For FunQ, this does **not** mean "add a control operator to the source IR next".
The relevant lesson is narrower:
- explicit local-control structure is valuable partly because it reduces the
  need for backend repair heuristics
- when introducing a join-aware emitter path, prefer a representation that
  makes context flow explicit instead of recovering it afterward

---

## Selective CPS is the main design-space insight

The paper's strongest general insight is that the interesting choice is often
**before or after CPS**, not **CPS or direct style forever**.

Their selective CPS translation:
- leaves already-exposed continuations alone
- translates only the parts of the program where continuations are still
  implicit
- preserves typing and meaning
- recovers an ordinary CPS-like stack discipline for the translated fragment

This is useful for FunQ mainly as an architectural reminder:
- a compiler can keep one dominant representation while adding a lighter-weight
  layer for the stage that needs more explicit control
- the backend refactor does not need to be all-or-nothing
- adding an emitter-facing join classification or small control-flow lowering
  layer is a valid midpoint, not a compromise born of incompleteness

---

## Separating non-returning from non-escaping is broader than continuations

Another important contribution is separating two properties that are often
conflated:
- **non-returning**: a continuation/jump has type `T -> ⊥`
- **non-escaping**: a value is 2nd-class and cannot leave its defining scope

This lets the paper treat continuations and user-defined local functions with
more precision:
- continuations are non-returning and non-escaping
- some user functions may be returning but still non-escaping
- both can therefore avoid heap allocation

This is more general than the immediate FunQ need, but it sharpens one planning
question. If FunQ later grows a more explicit backend-facing IR, "join point"
and "non-escaping local helper" do not have to be the same concept. Right now,
only the join-point part is immediately relevant.

---

## Evaluation and engineering lessons

The paper evaluates the approach in two very different settings:
- LMS: local labels/gotos in generated C for a regexp compiler yielded about a
  23% speedup on a benchmark mix
- MiniScala: exposing 2nd-class functions reduced allocation substantially on
  several benchmarks, with asymptotically meaningful improvements in cases like
  `pidigits` and `list`

The relevant takeaway for FunQ is modest but important:
- exposing the right local control structure can have concrete payoff even
  without a whole-compiler redesign
- the paper treats representation choice as an engineering knob, not as a
  one-time ideological commitment

---

## Limits and non-transferable parts

This paper is less direct than Kelsey, Maurer, or Kennedy as implementation
guidance for the next FunQ change.

What does **not** transfer directly:
- FunQ already uses CPS as its main middle-end IR, so the paper's direct-style
  front-end motivation is less pressing
- the `C` operator calculus is a research IR, not a drop-in design for the
  current codebase
- much of the evaluation is about allocation behavior for local functions in
  Scala-like settings, not emitter structure for a quantum compiler

So this should not be read as justification for:
- replacing FunQ's CPS tree IR
- adding a broad new control-operator layer across the pipeline
- turning the current backend task into a general IR redesign

---

## FunQ-specific takeaways

### 1. The remaining backend work should stay narrowly scoped

This paper strengthens the argument that FunQ's next step should remain small:
- explicit branch/join lowering in the emitter
- local join classification where needed
- current fallback behavior preserved for unsupported shapes

### 2. "Selective explicitness" is the right mental model

FunQ does not need every continuation to become a first-class backend notion.
It needs only the control points that matter for code generation to be treated
explicitly.

### 3. A lightweight emitter-facing layer is legitimate

If the emitter becomes hard to reason about in raw CPS form, a small
emitter-facing control/join representation would be consistent with the paper's
main lesson. That would still be a staged refinement, not a representation
failure.

### 4. The paper is more about refactor boundaries than immediate mechanics

Kelsey is still the strongest paper for block/join lowering.
Maurer is still the strongest paper for explicit second-class joins.
Kennedy is still the strongest paper for staying productively in CPS.
Cong et al. are most useful for answering: **how far should this refactor go?**

---

## Concrete implementation guidance for FunQ

The practical message to carry forward is:
- keep the existing CPS middle end
- improve the emitter around explicit local joins first
- avoid turning the current task into a full IR migration
- consider a small emitter-side control-flow layer only if raw CPS emission
  stays awkward after explicit join handling lands

This paper is best treated as a **design-space governor**. It supports an
incremental backend refactor and argues against forcing the compiler into a
false binary choice between "all CPS everywhere" and "no CPS at all."
