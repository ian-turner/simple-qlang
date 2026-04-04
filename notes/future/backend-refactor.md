# Backend Refactor — CPS/SSA Reading Map

See also: [../passes/openqasm-emission.md](../passes/openqasm-emission.md), [../passes/cps-conversion.md](../passes/cps-conversion.md)

---

## Problem to solve

Current state:
- `src/ToCPS.hs` introduces named join continuations for `LSwitch`
- `src/OpenQASM.hs` still emits by recursively interpreting CPS; branch
  merging relies on suffix hoisting (`splitCommonSuffix`) rather than explicit
  join handling

Refactor target:
- Lower branch/control structure explicitly rather than recovering it after
  per-arm interpretation
- Distinguish local join points from ordinary callable functions where useful
- Preserve the current emitter as a fallback while landing a more principled
  path for `if/else`-style joins

---

## Papers and where they fit

### 1. `resources/kelsey-ssa-cps.pdf`

**Paper:** Richard Kelsey, *A Correspondence between CPS and Static Single-Assignment Form*

**Most directly useful.** Key fit:
- Treat continuation parameters like phi/block parameters
- Treat continuations as basic blocks rather than ordinary functions in the backend
- Replace "run each branch to completion" with explicit branch edges and joins

**Expected payoff:** The cleanest justification for turning local join
continuations into backend blocks without needing a full SSA conversion pass.

### 2. `resources/maurer-compiling-without-continuations.pdf`

**Paper:** Maurer, Downen, Ariola, Peyton Jones, *Compiling without Continuations*

**Best reference for making join points explicit and second-class.** Key fit:
- Separate local control-flow joins from full callable functions
- Give joins dedicated IR status
- Avoid recovering shared tails with heuristics after the fact

**Expected payoff:** Most relevant source for the "don't compile branch-local
joins as ordinary calls" design decision.

### 3. `resources/kennedy-compiling-with-continuations-continued.pdf`

**Paper:** Andrew Kennedy, *Compiling with Continuations, Continued*

**Useful for keeping CPS as the backend representation while improving join handling.** Key fit:
- Contification-style thinking
- Separate continuation calling conventions from ordinary function calling
- Preserve CPS where convenient while still lowering joins efficiently

**Expected payoff:** Supports a staged approach: improve the backend around
explicit continuations first, without requiring an immediate full abandonment
of CPS.

### 4. `resources/cong-whatever.pdf`

**Paper:** Cong, Osvald, Essertel, Rompf, *Compiling with Continuations, or without? Whatever.*

**Design-space paper — scope control.** Key fit:
- Compare CPS-heavy vs join-point-heavy vs mixed IR strategies
- Justify an incremental migration rather than an all-at-once backend rewrite

**Expected payoff:** Useful for making sure the refactor remains appropriately
sized and does not grow into a full compiler IR redesign.

### 5. `resources/appel-continuations.pdf`

**Paper:** Andrew Appel, *Compiling with Continuations*

**Foundational context for the CPS side already in FunQ.** Use during planning
to cross-check that new backend handling still respects CPS tree semantics
produced by `src/ToCPS.hs`.

---

## Recommended reading order

1. `resources/kelsey-ssa-cps.pdf`
2. `resources/maurer-compiling-without-continuations.pdf`
3. `resources/kennedy-compiling-with-continuations-continued.pdf`
4. `resources/cong-whatever.pdf`
5. `resources/appel-continuations.pdf` as background/reference while coding

---

## Implementation passes

### Pass 1 — Named join continuations at the CPS level *(done)*

Primary references: Kelsey, Maurer

- Named join continuation in `src/ToCPS.hs` for `LSwitch` — done (commit `cb490fe`)
- Current recursive emitter path preserved as fallback — done
- `src/OpenQASM.hs` still uses suffix hoisting; explicit branch/join lowering
  in the emitter is the remaining work

### Pass 2 — Explicit join handling in the emitter *(planned)*

Primary references: Kelsey, Kennedy

Target:
- Thread branch-produced values through explicit join parameters in the emitter
- Run shared continuation code once after the branch instead of relying on
  suffix hoisting
- Validate nested `if/else` and measurement-controlled examples

### Pass 3 — Generalize beyond `LSwitch` *(planned)*

Primary references: Maurer, Cong

Target:
- Generalize beyond the minimal `LSwitch` case if warranted
- Decide how much of suffix hoisting should remain as a fallback optimization
- Document which `CFix` shapes are treated as backend joins vs ordinary callables

---

## Practical caution

The CPS-level join-continuation fix is done. The remaining work is narrowly in
the emitter:
- Teach the emitter to recognize join-continuation calls as branch exits
- Do not attempt a whole-backend replacement in the same pass
- Keep current emitter behavior available for unsupported shapes
