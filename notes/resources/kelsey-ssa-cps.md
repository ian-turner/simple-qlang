# Kelsey — *A Correspondence between CPS and SSA*

Richard A. Kelsey, 1995. Status: **initial notes**.

See also: [index.md](index.md), [../future/backend-refactor.md](../future/backend-refactor.md), [../passes/openqasm-emission.md](../passes/openqasm-emission.md)

---

## Why this paper matters first

This is the cleanest immediate reference for the emitter work already queued in
FunQ: branch-local join continuations should be treated as backend control-flow
joins, not as ordinary callable functions. The paper's main value is not "SSA
is better than CPS"; it is that a useful restricted fragment of CPS already has
an SSA-shaped control-flow interpretation.

---

## Core claim

Kelsey gives syntactic translations:
- restricted annotated CPS -> SSA
- SSA -> annotated CPS

The key restriction is to distinguish three roles that plain CPS often blurs:
- `Aproc`: a full procedure, called with a continuation and compiled with an
  ordinary call/return mechanism
- `Acont`: a return continuation for a call to a full procedure
- `Ajump`: a local continuation used only within the current procedure, suitable
  for compilation as a jump / labeled block

Once that distinction is made, many CPS programs are already "almost SSA". The
important idea for FunQ is that join continuations are not semantically the same
kind of thing as ordinary top-level callables.

---

## CPS -> SSA mapping

The CPS-to-SSA translation is straightforward because both representations are
single-assignment:
- `let` and inline continuation bindings become SSA assignments
- calling the current procedure's continuation variable becomes `return`
- calling an `Ajump`-bound variable becomes `goto` to a labeled block
- `Ajump` parameters become the labeled block's incoming parameters, i.e. the
  same role usually played by phi/block parameters

Two consequences matter for FunQ:
- branch joins should be represented explicitly as control-flow edges plus join
  parameters, not recovered after the fact by shared-suffix heuristics
- the values carried out of each branch belong to the join node, not to some
  invented callable function interface

Kelsey writes the SSA side with "C-functions" at block headers rather than
modern phi syntax, but the operational role is the same.

---

## SSA -> CPS mapping and dominance

The reverse direction is almost syntax-directed, except for one real issue:
where to place the `letrec` that binds each `Ajump`.

Kelsey uses the dominator tree of the SSA procedure:
- each labeled block is converted back into an `Ajump`
- the binding for that `Ajump` is inserted at the immediate dominator of the
  block

The conceptual point is useful beyond the actual translation:
- SSA dominance corresponds to lexical scope in CPS
- a local join continuation should live at the smallest enclosing scope that
  dominates all of its uses

That is a good mental model for deciding which `CFix`-bound continuations in
FunQ are backend-local joins versus real callable entities.

---

## Loops as recursive procedures

The paper's second major contribution is the procedure-merging transformation.
Functional programs often express loops as recursion, which makes useful flow
information look interprocedural. Kelsey shows a restricted but practical way to
merge procedures when they are always called with the same continuation:
- substitute that common continuation for each procedure's continuation variable
- remove the continuation parameter from those procedures
- turn the procedures from `Aproc` into `Ajump`
- bind them inside one larger enclosing procedure

After that transformation, the recursive structure becomes a single-procedure
control-flow graph, so ordinary intraprocedural SSA reasoning can see the loop.

For FunQ this is a strong argument for keeping local join/loop structure local
in the emitter instead of pretending everything is a separate callable unit.

---

## Limits of the correspondence

The mapping is not for arbitrary CPS:
- it depends on the `Aproc` / `Acont` / `Ajump` distinction being valid
- it assumes continuations are used in a stack-like last-in/first-out manner
- non-local control transfer such as `call/cc`-style continuation capture does
  not fit this correspondence cleanly

This limitation is acceptable for FunQ. The current compiler does not have
first-class continuation capture or arbitrary non-local exits, so the paper's
restricted setting is a good fit.

---

## FunQ-specific takeaways

### 1. `LSwitch` join continuations should lower as blocks

`src/ToCPS.hs` already introduces named join continuations for `LSwitch`. That
is exactly the shape this paper says to interpret as labeled blocks with join
parameters, not as ordinary function calls.

### 2. Emitter join handling should be explicit

`src/OpenQASM.hs` currently evaluates branch arms recursively and then tries to
recover a shared suffix. Kelsey points toward the cleaner model:
- treat each branch as producing arguments for a join
- treat the join as one shared continuation body
- run that shared body once after the branch

### 3. A full SSA IR is not required for the first refactor

The paper's practical lesson is lighter-weight than "invent a new backend IR".
FunQ can keep CPS as the middle-end representation and still lower a useful
subset of continuations as backend blocks.

### 4. Join classification should be structural

The backend should classify a `CFix`-bound continuation as a local join only
when:
- all uses are tail calls within the enclosing emitted function
- it is never passed as a value
- it is not part of the external callable interface

That classification matches the paper's `Ajump` role much better than the
current "everything callable looks function-like" fallback.

---

## Concrete implementation guidance for FunQ

Near-term backend direction:
- preserve the current CPS IR and existing emitter fallback
- recognize branch-local join continuations explicitly in the emitter
- treat join parameters like block parameters / phi inputs
- emit shared continuation code once, after the branch, instead of depending on
  suffix hoisting for the primary path

This is the right first reading before Maurer or Kennedy because it gives the
smallest defensible step from the current code to an explicit-join backend.
