# Backend Refactor Reading Map

This note maps the downloaded compiler papers in `resources/` onto the planned
OpenQASM backend refactor.

## Problem this reading set is meant to solve

Current state:

- `src/ToCPS.hs` now introduces named join continuations for `LSwitch`
- `src/OpenQASM.hs` still emits by recursively interpreting CPS
- dynamic branch merging still relies on suffix hoisting instead of explicit
  join handling

The backend refactor target is:

- lower branch/control structure explicitly instead of recovering it after
  per-arm interpretation
- distinguish local join points from ordinary callable functions where useful
- preserve the current emitter as a fallback while landing a more principled
  path for `if/else`-style joins

## Primary papers and where they fit

### 1. `resources/kelsey-ssa-cps.pdf`

**Paper**: Richard Kelsey, *A Correspondence between Continuation Passing Style
and Static Single Assignment Form*

**Why it matters here**

This is the most directly useful bridge from FunQ's existing CPS IR to a
backend block/join representation.

Key fit for the refactor:

- treat continuation parameters like phi/block parameters
- treat continuations as basic blocks rather than ordinary functions in the
  backend
- replace "run each branch to completion" with explicit branch edges and joins

**Use it for**

- planning pass: define the backend control-flow IR
- implementation pass 1: choose block parameter representation for branch
  results
- implementation pass 2: lower `CSwitch` plus join continuations into explicit
  join nodes

**Expected payoff**

This paper gives the cleanest justification for turning local join
continuations into backend blocks without needing a full SSA conversion pass.

### 2. `resources/maurer-compiling-without-continuations.pdf`

**Paper**: Maurer, Downen, Ariola, Peyton Jones,
*Compiling without Continuations*

**Why it matters here**

This paper is the best reference for making join points explicit and
second-class instead of pretending they are ordinary functions.

Key fit for the refactor:

- separate local control-flow joins from full callable functions
- give joins dedicated IR status
- avoid recovering shared tails with heuristics after the fact

**Use it for**

- planning pass: decide which `CFix` bindings should become backend join blocks
- implementation pass 1: add a join/block node or join-classification step
- implementation pass 3: widen coverage beyond the exact `LSwitch` shape if
  the first version lands cleanly

**Expected payoff**

This is the most relevant source for the "don’t compile branch-local joins as
ordinary calls" design decision.

### 3. `resources/kennedy-compiling-with-continuations-continued.pdf`

**Paper**: Andrew Kennedy, *Compiling with Continuations, Continued*

**Why it matters here**

Useful if FunQ keeps CPS as the backend-facing representation for a while but
wants a principled distinction between function calls and non-returning local
continuations.

Key fit for the refactor:

- contification-style thinking
- separate continuation calling conventions from ordinary function calling
- preserve CPS where convenient while still lowering joins efficiently

**Use it for**

- planning pass: decide whether the backend IR should still look CPS-like
- implementation pass 2: define how join calls are recognized and lowered
- later cleanup: avoid over-generalizing the first join-point implementation

**Expected payoff**

This paper supports a staged approach: improve the backend around explicit
continuations first, without requiring an immediate full abandonment of CPS.

### 4. `resources/cong-whatever.pdf`

**Paper**: Cong, Osvald, Essertel, Rompf,
*Compiling with Continuations, or without? Whatever.*

**Why it matters here**

This is a design-space paper.  It is less likely to dictate the exact FunQ
implementation, but it is useful for avoiding false binary choices.

Key fit for the refactor:

- compare CPS-heavy vs join-point-heavy vs mixed IR strategies
- justify an incremental migration rather than an all-at-once backend rewrite

**Use it for**

- planning pass: scope control and tradeoff framing
- implementation review: sanity-check whether the chosen backend boundary is
  too elaborate for current FunQ needs

**Expected payoff**

This paper is mainly useful for making sure the refactor remains appropriately
sized and does not grow into a full compiler IR redesign unless that becomes
necessary.

### 5. `resources/appel-continuations.pdf`

**Paper**: Andrew Appel, *Compiling with Continuations*

**Why it matters here**

This remains the reference for the CPS side of the compiler already present in
FunQ.

Key fit for the refactor:

- reinforce the meaning of `CFix`, tail calls, and continuation structure
- cross-check that new backend handling still respects the semantics of the
  CPS tree produced by `src/ToCPS.hs`
- provide precedent for naming continuations instead of inlining them

**Use it for**

- background during planning
- verifying the new `LSwitch` join-continuation lowering
- keeping terminology and invariants straight while refactoring the backend

**Expected payoff**

This is foundational context, but not the main guide for the backend control
rewrite itself.

## Recommended reading order for the current work

1. `resources/kelsey-ssa-cps.pdf`
2. `resources/maurer-compiling-without-continuations.pdf`
3. `resources/kennedy-compiling-with-continuations-continued.pdf`
4. `resources/cong-whatever.pdf`
5. `resources/appel-continuations.pdf` as background/reference while coding

## How this maps onto a pragmatic FunQ implementation plan

### Planning pass

Read Kelsey first, then Maurer, then skim Kennedy.

Output expected from the planning pass:

- a backend IR sketch for explicit branch/join lowering
- a rule for recognizing local join continuations generated from `LSwitch`
- a decision on whether join points become block-like nodes or a classified
  subset of `CFix`
- a scoped fallback strategy for everything the new path does not yet cover

### Implementation pass 1

Primary references: Kelsey, Maurer

Target:

- isolate `CSwitch` lowering in `src/OpenQASM.hs`
- add explicit branch/join lowering for the exact named-join shape now emitted
  by `src/ToCPS.hs`
- preserve the current recursive emitter path as fallback

### Implementation pass 2

Primary references: Kelsey, Kennedy

Target:

- thread branch-produced values through explicit join parameters
- run shared continuation code once after the branch instead of relying on
  suffix hoisting
- validate nested `if/else` and measurement-controlled examples

### Implementation pass 3

Primary references: Maurer, Cong

Target:

- generalize beyond the minimal `LSwitch` case if warranted
- decide how much of suffix hoisting should remain as a fallback optimization
- document which `CFix` shapes are treated as backend joins vs ordinary
  callables

## Practical caution

These papers are useful because the current problem is specifically about local
join structure in CPS.  Generic SSA or code-generation textbooks are less
likely to help with the immediate FunQ issue than these control-flow-specific
references.

The first implementation should stay narrow:

- fix `if/else` branch joins
- do not attempt a whole-backend replacement in the same pass
- keep current emitter behavior available for unsupported shapes
