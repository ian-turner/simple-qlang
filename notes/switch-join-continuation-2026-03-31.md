# Named Join Continuations for Switch

## What changed

`src/ToCPS.hs`: the `LSwitch` case now introduces a named join continuation
instead of passing the outer continuation `c` directly into each arm.

### Before

```
F(switch scrut arms, c) =
  F(scrut, λv. SWITCH v [F(armᵢ, c)])
```

The Haskell-level continuation `c` was applied inside every arm, physically
inlining the post-switch code into each branch of the resulting `CSwitch` node.
For a two-arm boolean switch the post-switch continuation appeared twice in the
CPS tree.

### After

```
F(switch scrut arms, c) =
  FIX [(j, [x], c(x))]
    (F(scrut, λv. SWITCH v [F(armᵢ, λr. APP(j,[r]))]))
```

The continuation `c` is placed in `j`'s body exactly once.  Each arm
tail-calls `j` with its result instead of inlining `c` directly.

## Why this is safe end-to-end

### RecElim

`j`'s body is `c (VVar x)`.  `c` is a Haskell-level function passed in from
the call site; it is constructed before `j` is created and therefore cannot
reference `j`.  `calleesInExp (c (VVar x))` will never return `j`, so
`RecElim` passes the group without error.

### Closure conversion

`j` is a standard single-function `CFix` group.  `ClosureConv` handles it
identically to any other locally defined function: it computes the free
variables of `j`'s body, builds a shared closure record, and lifts `j` to the
top-level `CFix`.  The `CSwitch` arms reference `j` as a `VVar` in the
`CFix`'s own body, so `j` is not counted as a free variable of the group.

### All subsequent passes (Defunc, QubitHoist, RecordFlatten, GateDef)

Every pass traverses `CFix` uniformly without special-casing the number of
definitions or their shape.  A join-continuation `CFix` is indistinguishable
from any other single-function group.

### OpenQASM emitter

The emitter is an interpreter.  When it reaches a `CSwitch` with a
`ClassicalVar` scrutinee it runs each arm independently to completion via
`runExp`.  Each arm ends with a closure-converted call to `j`; the emitter
resolves this via `applyCallable (CallableFun ...)` and inlines `j`'s body.
Both arms therefore produce statement lists that end with the identical
continuation code, and `splitCommonSuffix` hoists it out of the `if/else`
block exactly as before.

For constant scrutinees the emitter takes the `ClassicalIntConst n` branch and
runs only the matching arm.  The arm calls `j`, the emitter inlines `j`'s
body, and execution continues normally.

## What this does and does not fix

**Fixed**: the CPS intermediate representation no longer duplicates the
post-switch continuation.  The continuation is defined once as `j`'s body and
referenced (as a tail call) from each arm.

**Not fixed**: the OpenQASM emitter still inlines `j`'s body into each arm
during interpretation.  The `splitCommonSuffix` heuristic is still required to
deduplicate the emitted statements.  Eliminating that duplication from the
output would require teaching the emitter to recognise `j`-calls as branch
exits and run `j`'s body only once after the `if/else` block.  That is a more
invasive emitter change and is deferred.

## Pattern precedent

This is exactly the same pattern used by `LApp` (§5.4 in `ToCPS.hs`), which
also wraps a fresh `CFix [(r, [x], c(x))]` around the call expression so that
the result continuation is named rather than inlined.  The join-continuation
approach for `LSwitch` follows that established idiom.
