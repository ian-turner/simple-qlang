# Maurer et al. — *Compiling without Continuations*

Luke Maurer, Paul Downen, Zena M. Ariola, Simon Peyton Jones, 2017. Status: **initial notes**.

See also: [index.md](index.md), [kelsey-ssa-cps.md](kelsey-ssa-cps.md), [../future/backend-refactor.md](../future/backend-refactor.md), [../passes/openqasm-emission.md](../passes/openqasm-emission.md)

---

## Why this paper matters after Kelsey

Kelsey explains why many CPS continuations already have an SSA/block-shaped
reading. Maurer et al. answer the next question: how do you preserve and exploit
that join structure throughout optimization, instead of only rediscovering it
late in code generation?

That is directly relevant to FunQ's current backend situation:
- `src/ToCPS.hs` already creates named join continuations for `LSwitch`
- `src/OpenQASM.hs` still mostly treats control by recursive interpretation and
  shared-suffix recovery
- the remaining work is not "invent a whole new IR", but "treat local joins as
  a distinct control construct and stop compiling them like ordinary callables"

---

## Core claim

The paper argues that direct-style ANF can recover the important optimization
benefits often associated with CPS by adding **explicit join points** and
**jumps** as second-class control constructs.

The central design point is simple:
- ordinary functions may escape, be partially applied, and behave like values
- join points are local control nodes
- jumps to join points are saturated tail calls only
- join points should therefore be optimized and compiled differently from
  ordinary functions

For FunQ, the important lesson is not "switch from CPS to ANF". FunQ already
has a CPS middle end, so the direct-style motivation is secondary. The useful
lesson is the same separation Kelsey points toward: **do not blur local control
joins and real callable functions into one undifferentiated notion of call**.

---

## System FJ essentials

The paper formalizes this idea with two new constructs:
- `join`: introduces a local, possibly recursive join point
- `jump`: transfers control to that join point

Important invariants:
- join points are **polyadic and saturated**; partial application is not allowed
- jumps must appear only where control can safely abandon the current
  evaluation context
- join points may be **recursive**, so loops can be represented directly as
  local control flow rather than heap-allocated local functions

The type system enforces the operational intuition: a jump is not an ordinary
call that returns to its caller. It abandons intermediate evaluation context and
resumes at the join binding's return point.

For FunQ this reinforces a useful backend rule:
- a continuation/join call should not be modeled as an ordinary callable
  function invocation if it is really just a branch or loop edge

---

## The main optimizer lesson: preserve join-ness explicitly

The most important contribution for FunQ is not the calculus itself but the
transformation discipline around it.

The paper's argument is that recognizing join points only in the backend is too
late. Earlier optimizations may already have destroyed the shape that made the
binding a join point in the first place.

The key rewrite intuition is:
- when a transformation crosses a join binding, push the surrounding context
  into the join body (`jfloat`)
- when a transformation crosses a jump, discard the surrounding context
  (`abort`)

This is what lets commuting conversions preserve join points instead of turning
them back into ordinary function-like bindings.

That matters to FunQ because the current emitter still recovers control joins
after the fact with suffix-hoisting heuristics. Maurer et al. provide the right
design pressure in the other direction:
- represent the join explicitly
- preserve it through transformations
- compile it as control flow, not as a normal callable

---

## Contification

The paper also gives a lightweight **contification** pass: infer that an
ordinary let-bound function is really a join point when:
- every call is saturated
- every call is in tail position
- the binding does not escape into value position
- the function body matches the surrounding result type constraints

Two practical points stand out:
- the analysis is deliberately simple because it only looks for tail calls
- it can be rerun frequently as optimization changes the program shape

For FunQ this is useful in a slightly different way than in GHC:
- FunQ already generates many continuations explicitly, so some join structure
  exists from the start
- the remaining problem is classification: which `CFix`-bound labels should be
  treated as backend-local joins/blocks rather than as ordinary call targets?
- Maurer suggests that a simple structural criterion is often enough

This strengthens the case for an emitter-side or late-middle-end join analysis
that checks:
- local scope
- tail-call-only use sites
- no value escape
- saturated application only

---

## Recursive join points and loops

The paper emphasizes that join points can be recursive. This matters because a
recursive local helper is often really a loop, not a heap-allocated function.

Their fusion example is specific to GHC, but the structural lesson transfers:
- once a recursive helper is known to be a join point, consumers can commute
  inward to the helper's return points
- that exposes simplifications that are hard to recover if the helper remains
  an ordinary local function

For FunQ:
- self-recursive tail loops already exist in the emitted backend path
- branch-local join continuations from `LSwitch` are the immediate case
- longer term, some recursive `CFix` members may want classification as local
  loop/control nodes instead of callable entities

This paper is not the primary reference for bounded recursion lowering, but it
does support the more local claim that **recursive control structure should stay
local in the IR/back end when it does not need full function semantics**.

---

## Implementation lessons from GHC

The GHC experience report is especially useful because it shows how small the
representation change can be:
- they did **not** need a wholly separate AST for join bindings
- ordinary let-bindings and applications were reused, with join-ness tracked as
  metadata on identifiers
- an internal checker (`Core Lint`) was extended to catch passes that destroy
  join-point invariants

The pass-level lessons are concrete:
- simplification must know how to push context into joins and discard context at
  jumps
- float-out can destroy join points by moving them too far outward
- float-in can destroy join points by unsaturating them
- downstream transforms should preserve join classification, not silently erase
  it

This is a good fit for FunQ's likely refactor style: start with a lightweight
representation change and targeted backend logic, not a full IR redesign.

---

## Limits and non-transferable parts

Not everything in the paper carries over directly:
- much of the paper's pitch is "keep direct style instead of switching to CPS";
  FunQ is already on the CPS side of that decision
- the fusion case study is about lazy list-stream optimization in GHC, not about
  quantum compilation
- the formal typing story is framed for System F / ANF rather than an Appel-like
  CPS tree IR

So the direct import is narrower:
- explicit second-class joins
- preservation of join structure through transformations
- lightweight contification/classification
- recursive joins as local loops

---

## FunQ-specific takeaways

### 1. Join continuations should be treated as a separate class of entity

FunQ currently has a semantic distinction without a fully explicit backend one:
- some labels are genuine callable procedures/functions
- some labels are just local branch or loop joins

Maurer et al. support making that distinction explicit.

### 2. Late recognition is not enough

If the backend only notices joins after recursive emission has already happened,
it will keep needing repair heuristics like suffix hoisting. The paper argues
for preserving join structure earlier and compiling it directly.

### 3. A small representation change is probably sufficient

FunQ may not need a new global CFG IR for the first backend refactor. A smaller
step is likely enough:
- annotate/classify certain `CFix` bindings as local joins
- or add a dedicated join/jump form in an emitter-facing IR layer

Either approach would align with the paper's "high power-to-weight ratio"
message.

### 4. Recursive local control should stay local

The paper strengthens the intuition that local recursive control flow should not
be forced through the same path as externally callable code if it never escapes.

### 5. This complements Kelsey rather than replacing it

Kelsey gives the SSA/block correspondence for CPS continuations.
Maurer et al. explain how to preserve and infer join structure in an optimizer.
Together they support a staged FunQ plan:
- classify local joins distinctly
- keep CPS as the middle-end representation
- lower local joins as backend control-flow blocks/edges

---

## Concrete implementation guidance for FunQ

Near-term backend direction suggested by this paper:
- keep the current CPS IR as the main middle-end representation
- add a lightweight join classification for `CFix`-bound labels
- require that classified joins are local, tail-called, saturated, and never
  passed as values
- teach the emitter to compile those joins as structured control flow rather
  than normal callable targets
- preserve the current fallback path for unsupported shapes

This is the strongest practical idea to carry forward from the paper: **make
join-ness explicit enough that later passes can preserve it, but do not pay for
a full CPS replacement or whole-compiler IR redesign just to get that win**.
