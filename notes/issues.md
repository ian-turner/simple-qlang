# Known Issues

Observed bugs and known limitations in the current compiler. Linked to the
relevant pass notes where applicable.

---

## IR bloat from un-eliminated identity continuations

**Observed in:** record-flattened IR (visible with `--debug`), bell00 and similar programs  
**Relevant pass:** [passes/cps-conversion.md](passes/cps-conversion.md)

CPS conversion generates eta-wrappers of the form `fix r(x) = k(x) in ...`
for each intermediate continuation binding. These survive unchanged through every
subsequent pass and appear as full closure-entry-point functions in the
record-flattened IR — each requiring a closure record allocation, a dispatch
switch, and a closure field load just to forward arguments to the next
continuation.

For `bell00` (3 meaningful operations) the record-flattened IR contains 5
states; 2 are pure forwarding continuations with no semantics. For `output`
(4 operations) the count is 7 with 3 such wrappers.

The fix is **beta-contraction** (Appel Ch. 6): when the body of a `CFix`
binding is a tail call to another known function, inline the call and drop the
binding. This would reduce bell00 from 5 states to ~3 and proportionally
shrink the dispatch table (see below).

This is a deferred optimization — see pipeline stage note in
[../CLAUDE.md](../CLAUDE.md).

---

## Module-wide defunctionalization dispatch fan-out

**Observed in:** defunctionalized and record-flattened IR for any program with
multiple closures  
**Relevant pass:** [passes/defunctionalization.md](passes/defunctionalization.md)

`Defunc.hs` builds a single global tag map from *all* synthetic closure labels
in a declaration and uses it at *every* dispatch site. This means each
`switch _cp { ... }` has arms for every closure tag in the module — most of
which are unreachable from that specific call site. For bell00+output the
dispatch tables are 5 and 7 arms wide respectively, when in practice each site
only ever takes one arm.

The table width scales linearly with the number of closures in the program.
For large programs this is a correctness-preserving but costly redundancy.

The note in `passes/defunctionalization.md` already documents this as a known
limitation. The long-term fix is per-call-site dispatch set analysis (only
include labels whose type is compatible with the call site) or the standard
shared-`apply`-function approach. Eliminating the identity continuation
closures (see above) would also directly reduce the table width.

---

## `RecordFlatten` keeps dead `COffset` bindings when source is opaque

**Observed in:** record-flattened IR — specifically the leading
`let f = offset 0 clo` in each closure entry point  
**Relevant pass:** [passes/record-flattening.md](passes/record-flattening.md)  
**Source:** `src/RecordFlatten.hs:49`

`flattenExp` handles `COffset` in two branches:

- `Just atom` — forward the variable through the env; drop the binding if it
  turns out to be dead.
- `Nothing` — source record is unknown (e.g. a function parameter); keep the
  binding **unconditionally**, without checking liveness.

This means that when a closure entry point opens with
`let f = offset 0 clo` (where `clo` is a parameter and therefore opaque), the
binding is preserved even when `f` is immediately shadowed by a later binding
or never used. In the bell00 debug output every state has exactly this dead
binding at the top.

Fix: in the `Nothing` branch, check `occursInExp v body'` after flattening the
body, and drop the `COffset` if the variable is dead, mirroring the `Just`
branch logic.

```haskell
-- current (RecordFlatten.hs ~line 49)
Nothing ->
  COffset off val' v (flattenExp env' body)

-- proposed
Nothing ->
  let body' = flattenExp env' body
  in if occursInExp v body'
       then COffset off val' v body'
       else body'
```

---

## Free `_cp` variable left by defunctionalization

**Observed in:** defunctionalized IR onward — every `CSwitch` produced by
`Defunc.hs`  
**Relevant pass:** [passes/defunctionalization.md](passes/defunctionalization.md)  
**Source:** `src/Defunc.hs:45–49`, `src/Defunc.hs:92–97`

`Defunc` pattern-matches and consumes the entire expression
`CSelect 0 clo tag (CApp (VVar tag) (clo : args))`, replacing it with
`CSwitch (VVar tag) [direct calls]`. The `CSelect` that bound `tag` is
absorbed into the pattern — it is not emitted into the output. The variable
`tag` (`_cp@x` in debug output) therefore appears **free** in the resulting
`CSwitch`, which is a scope violation in the CPS IR.

The emitter works around this: it resolves the free variable through its own
value-tracking environment, which retains the integer tag from the `CRecord`
built just above the call site. But this is fragile — any intermediate pass
that depends on the CPS term being well-scoped (e.g., dead-variable analysis,
substitution, or a future liveness pass) will be confused by the free variable.

The clean fix is to have `RecordFlatten` (or a separate peephole pass)
substitute the known tag integer for `_cp@x` at each `CSwitch` site. By the
time `RecordFlatten` runs, the preceding `CRecord` and `COffset` bindings give
enough static information to resolve the tag:

```
let rec = record [2, ...] in
  let f = offset 0 rec in
    switch _cp { ... }   -- _cp was f.0 = 2; substitute switch 2 { ... }
```

After substitution with a known integer, the emitter's `evalSwitchScrutinee`
can eliminate the switch entirely in favor of a direct call to the single
matching arm.
```
