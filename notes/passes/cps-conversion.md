# CPS Conversion

**Module:** `src/ToCPS.hs`
**Input:** `LExp` (Lambda IR from `Lower.hs`)
**Output:** `CExp` (CPS IR)

See also: [../pipeline.md](../pipeline.md), [../resources/appel/ch05-cps-conversion.md](../resources/appel/ch05-cps-conversion.md)

---

## Overview

Following Appel Ch 5, every function in FunQ gains an extra continuation
argument `c`. "Returning a value `v`" is rewritten as `APP(c, [v])`. The
transformation ensures all calls are tail calls; there is no implicit stack.

---

## Named result continuation for `LApp`

When converting a function application `e1 e2`, the result must be passed to
the outer continuation `c`. Rather than inlining `c` directly into the call,
`ToCPS` wraps a fresh `CFix` around the call so the result continuation is
named:

```
FIX [(r, [x], c(x))]
  APP(e1, [e2, r])
```

This is Appel's §5.4 pattern. It ensures the continuation is a `VVar`
reference rather than a structurally-embedded closure, which keeps the CPS
tree well-formed for downstream passes.

---

## Named join continuation for `LSwitch`

When converting a `switch scrut arms` expression, a naive translation would
inline the outer continuation `c` into every arm:

```
-- Naive (pre-fix)
F(switch scrut arms, c) =
  F(scrut, λv. SWITCH v [F(armᵢ, c)])
```

This duplicates `c` — for a two-arm boolean switch, the post-switch code
appears twice in the CPS tree. Even though the two copies are semantically
identical, downstream passes and the emitter see them as separate trees and may
generate duplicate OpenQASM.

The current implementation introduces a named join continuation `j` instead:

```
F(switch scrut arms, c) =
  FIX [(j, [x], c(x))]
    (F(scrut, λv. SWITCH v [F(armᵢ, λr. APP(j,[r]))]))
```

`c` appears exactly once, in `j`'s body. Each arm tail-calls `j` with its
result. The same pattern is used by `LApp` and is the established idiom in this
codebase.

### Safety through the pipeline

| Pass | Why `j` is safe |
|---|---|
| `RecElim` | `j`'s body is `c(VVar x)`; `c` is constructed before `j` and cannot reference `j`, so no recursion cycle |
| `ClosureConv` | `j` is a standard single-function `CFix` group; treated identically to any other local function |
| `Defunc`, `QubitHoist`, `RecordFlatten`, `GateDef` | All traverse `CFix` uniformly; join-continuation groups are indistinguishable from other single-function groups |
| OpenQASM emitter | Resolves `j`-calls via `applyCallable`; inlines `j`'s body into each arm; `splitCommonSuffix` hoists the identical suffix |

### What this does and does not fix

**Fixed:** the CPS intermediate representation no longer duplicates the
post-switch continuation. The continuation is defined once as `j`'s body and
referenced (as a tail call) from each arm.

**Not yet fixed:** the OpenQASM emitter still inlines `j`'s body into each arm
during interpretation. The `splitCommonSuffix` heuristic deduplicates the
emitted statements. Eliminating that emitter-level duplication would require
recognizing `j`-calls as branch exits and running `j`'s body only once after
the `if/else` block — a more invasive emitter change deferred to the backend
refactor (see [../future/backend-refactor.md](../future/backend-refactor.md)).

---

## Measurement semantics

`meas` lowers to a single-result `CPrimOp`:

```
PRIMOP(measure, [VAR q], [b], [cont])
```

`b` is the classical `Bool` result; `cont` is the single continuation. Any
branching on `b` happens through a later `CSwitch` rather than being forced at
the measurement site.

An earlier encoding used two continuations `[zeroCont, oneCont]`. That was
abandoned because it forced a backend branch at every measurement, duplicating
downstream code even when the program would not branch on the result. See
[../quantum-semantics.md](../quantum-semantics.md).
