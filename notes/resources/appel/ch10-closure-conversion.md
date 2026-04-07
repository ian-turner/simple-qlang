# Chapter 10 — Closure Conversion

Pages 103–124. Status: **complete**.

---

## Overview

Closure conversion is the pass that eliminates all free variables from every
function body. After this pass, every function is *closed*: its only variables
are its explicit parameters and locally bound names. Closed functions can be
lifted to top level and referenced as bare code pointers (VLabel).

The pass operates on the CPS IR produced by Chapter 5 (and optionally polished
by the Chapter 6–9 optimizations). It produces a CPS expression in which:

- No FIX-defined function has any free variables other than the names of
  functions defined by the surrounding FIX.
- After the pass, all such FIX definitions are lifted to a single top-level
  FIX, and all references to the names they define use VLabel (not VVar).

---

## 10.1–10.2 Motivation and worked examples (p. 103–108)

A closure is a pair of:
- a *code pointer* (the machine-code address of the closed function), and
- a *closure record* (a heap object holding the values of free variables).

When a function `f` with free variable `x` is passed as a value, the caller
has no way to supply `x` at the call site. Instead, `f`'s closure record
carries `x`; when `f` is entered, it retrieves `x` by selecting from the
record.

Appel's example: the curried `add` function. `add` itself has no free
variables (trivial closure). `g = add 5` creates a closure for `fn y => x+y`
that stores `x=5`. When `g 10` is called, the code pointer is fetched from
field 0 of the closure, the argument 10 and the closure are passed; the body
fetches `x` from field 1 of the closure.

### Closure-passing style

After closure conversion, **every function is passed its own closure** as an
extra first argument. To call an escaping function stored in variable `c`:

1. Fetch the code pointer: `code = SELECT(0, c)`
2. Call `code(c, arg)` — the closure itself is the first argument

This means the function body always has its closure available for free-variable
extraction, regardless of where the closure came from.

### Shared closures within a FIX (mutual recursion)

Functions defined in the same FIX must be able to refer to each other. For
escaping mutually recursive functions, the standard solution is **closure
sharing**: all functions in one FIX share a single closure record. This is
*structurally required* for mutual recursion, not merely an optimization.
The shared record contains one code pointer per function plus all shared free
variables:

```
val closure = RECORD(code_f, code_g, fv1, fv2, ...)
val f = closure               -- field 0 is f's code
val g = OFFSET(closure, 1)    -- field 0 of g's "closure" is g's code
```

The OFFSET primitive makes a pointer into the middle of the shared record so
that field 0 of each function's nominal closure is the code pointer for that
function.

---

## 10.3 Closure-passing style (p. 109)

The closure-conversion pass rewrites the CPS expression into *closure-passing
style*:

- Every function that requires a closure gains an extra first parameter
  (conventionally named `self` or `env`) bound to its own closure at
  function entry.
- Every free variable `x` in a function body becomes `SELECT(i, self, x)`
  for the appropriate offset `i`.
- At each point where a closure is created (i.e., where a FIX is
  lexically present and the closure is first needed), a RECORD is built.

---

## 10.4 The closure-conversion algorithm (p. 109–112)

### Escaping vs known functions

The key distinction:

- **Escaping function**: appears as a *value* (stored in a record, passed as
  an argument to an escaping function, returned as a result). Its call sites
  are not all statically known. It must use a **standard closure**: code
  pointer at field 0, free variables at subsequent fields. The closure is
  passed as its first argument.

- **Known function**: all call sites are statically known (i.e., it only
  appears in function position of a CApp and is never passed as a value). Its
  free variables can be passed as **extra explicit arguments** directly, with
  no closure record needed (up to the register limit N).

### Free-variable sets for known functions

When a known function `f` calls another known function `g`, all of `g`'s free
variables become free in `f` (since `f` must supply them). This creates a
fixed-point computation: let `V₀(f)` be the initial free variables of `f`
and let `E(f)` be the set of known functions called by `f`:

```
V_i(f) = V_{i-1}(f) ∪ (⋃_{g ∈ E(f) ∩ K} V_{i-1}(g))
```

Iterate until fixed point. This is equivalent to live-variable dataflow
analysis in a conventional compiler.

### When known functions need closures

If a known function accumulates more than `N` free variables (where `N` is the
number of machine registers), it can no longer pass all of them explicitly; a
closure is required. We track the set `C_i` of functions that require a
closure:

```
C_0 = all escaping functions
C_i = C_{i-1}
    ∪ {f | N < |V_{i-1}(f)|}          -- too many free vars
    ∪ {f | C_{i-1} ∩ E(f) ∩ F(f) ≠ ∅} -- f calls a FIX-sibling that needs a closure
```

where `F(f)` is the set of functions defined in the same FIX as `f`. Once a
function is in `C`, it stays there (monotone). This converges quickly.

If `f` is known and calls an escaping sibling `g` in the same FIX, `f` uses
`g`'s shared closure rather than creating its own — it costs nothing extra.

---

## 10.5 Closure representation (p. 112–114)

### Flat closures (SML/NJ and FunQ choice)

A flat closure for function `f` with free variables `v₁, ..., vₙ` is a single
heap record:

```
RECORD(code_ptr_f, v₁, v₂, ..., vₙ)
```

**Advantages:**
- Any free variable is accessible in exactly one SELECT (fast).
- No pointer chains that could prevent garbage collection.

**Disadvantage:** if `f` and `g` share many free variables, both closures
contain copies of those values (but this is bounded by program size, not input
size — asymptotically fine).

### Linked closures (not used in FunQ)

A linked closure for `f` nested in `g` contains only `f`'s own local free
variables plus a pointer to `g`'s closure. Access to a variable bound by `g`
requires following the link. This can lead to O(N²) space for certain programs
(see §12.3 example) and can prevent GC of no-longer-needed data. **Not used.**

---

## 10.6 Callee-save registers (p. 114–118)

This section introduces an optimization for continuation closures. When `f`
makes a nontail call that creates a continuation `k`, `k` normally requires
a heap-allocated closure. Instead of heap-allocating, we can pass `k`'s free
variables as *extra register arguments* through the continuation convention:
each user function gains `n` extra callee-save arguments `c₀, c₁, ..., cₙ`
that it must pass on (unchanged) to its continuation.

This avoids heap-allocating continuation closures when free-variable sets are
small. SML/NJ uses this extensively.

**FunQ status**: Deferred. FunQ's initial closure conversion will not implement
callee-save register optimization. All continuation closures will use standard
flat closure records.

---

## 10.7 Callee-save continuation closures (p. 119–122)

Details the interaction between callee-save registers and escaping continuations.
Introduces the notion of *well-behaved* continuation variables (those that appear
only as the second argument of escaping user functions, as arguments to known
functions, or in function position).

**FunQ relevance**: FunQ has no `callcc` and no exception handlers. All
continuation variables in well-typed FunQ programs are well-behaved by
construction. No normalization step is required.

---

## 10.8 Stack allocation of closures (p. 122–124)

Functions that *escape downward only* (not upward — not stored into records,
not returned as results) can have their closures stack-allocated. SML/NJ
ultimately does **not** use this optimization because:

1. Heap allocation + copying GC is efficient enough.
2. The callee-save register optimization already eliminates most continuation
   closure heap allocations.

**FunQ status**: Not implemented. All closures are heap-allocated records.

---

## 10.9 Lifting function definitions to top level (p. 124)

After closure conversion, no function has any free variables. Therefore:

- All FIX definitions can be **lifted to a single top-level FIX**.
- The result is one flat program with no nested FIX definitions.
- All references to functions defined by any FIX use the **VLabel** constructor
  (a statically known code address), not VVar (a runtime register value).

This is important for subsequent phases: VLabels don't count as "free
variables" for register allocation or spilling purposes.

---

## FunQ Implementation Notes (Stage 4 — complete)

`src/ClosureConv.hs` implements the pass. `CPSExp.hs` was extended with
`COffset`. The current CLI no longer prints intermediate IR stages, but the
closure-converted form remains part of the compiled module pipeline.

### Design decisions made during implementation

- **All functions treated as escaping** (conservative). This avoids the known-function
  fixed-point computation and is correct; it just means every continuation gets a
  heap-allocated closure.
- **`COffset` added to CPSExp** — needed for mutual recursion; creates an interior
  pointer into a shared record so each function's field 0 is its own code pointer.
- **Shared preamble approach** — rather than substituting free variables inline, each
  lifted function body begins with a preamble of CSelect/COffset bindings that rebind
  every free variable and sibling by extracting them from the closure parameter.
- **Cross-declaration VLabel calls** (`CApp (VLabel "bell00") args`) pass through
  unchanged. These are calls to other top-level declarations compiled separately.
  The calling convention mismatch (missing closure arg) will be resolved at the
  linkage/emission stage.

---

## FunQ Implementation Plan (minimal viable closure conversion)

We implement the following subset of Chapter 10:

### What we implement

1. **Free variable computation** — a recursive pass over CExp collecting FV(e)
   for every subexpression.

2. **Escaping vs known classification** — a function is escaping if its name
   appears as a *value* (as a VVar argument to CApp, or inside a CRecord) rather
   than only in function position of a known-call CApp. In the first pass, err
   on the side of classifying functions as escaping.

3. **Flat closures for escaping functions** — for each escaping function `f`
   in a FIX, with free variables `{w₁, ..., wₙ}` (excluding FIX-sibling names):
   - At the creation site, build a RECORD `(code_ptr_f, w₁, ..., wₙ)`.
   - Add a closure parameter `env` as the new first formal parameter.
   - Rewrite each occurrence of `wᵢ` in the body to `SELECT(i, env, wᵢ)`.
   - All functions in the same FIX share one closure record (with OFFSET
     to give each function a clean field-0 code pointer).

4. **Known functions — free vars as extra args** — for each known function `f`,
   add its free variables as extra formal parameters; add the corresponding
   values as extra actual arguments at each call site.

5. **Qubit invariant** — for well-typed FunQ, no qubit variable should appear
   as a free variable of any function. The pass assumes this invariant holds;
   it does not enforce it at runtime. (The linear type checker will enforce it
   statically when implemented.)

6. **Top-level lifting (§10.9)** — after conversion, collect all FIX
   definitions into one top-level FIX. Replace all VVar references to
   function names with VLabel.

### What we defer

- Callee-save register optimization (§10.6–10.7)
- Stack allocation of closures (§10.8)
- Linked closures and other representation variants (§10.5)
- The iterative `C_i` fixed-point for closure necessity (we conservatively
  put all escaping functions in closures from the start)

### Input/output types

```haskell
-- Input:  CExp with free variables; FIX defs may be nested
-- Output: CExp with no free variables; all FIX defs at top level
closureConvert :: CExp -> CExp
```

The output CExp will have the form:

```
CFix [ (f₁, params₁, body₁),
       (f₂, params₂, body₂),
       ...
     ]
     mainExpr
```

where no `bodyᵢ` or `mainExpr` has free variables other than the `fᵢ` names,
and all `fᵢ` names are referenced via VLabel.

---

## Sections Not Relevant to FunQ

- **§10.6–10.7 Callee-save registers and continuation closures** — deferred
  optimization; not needed for correctness
- **§10.8 Stack allocation** — deferred optimization; heap allocation is fine
- Linked closures, merged closures (§12.3 review), skip lists,
  path compression — representation variants we won't use
- Exception handler machinery (scattered through §10.6–10.7) — FunQ has no
  exceptions
