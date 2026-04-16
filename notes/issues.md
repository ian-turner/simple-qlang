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
which are unreachable from that specific call site.

**Scope after previous fixes:** dispatch sites where the closure tag is
statically visible are now eliminated entirely by `RecordFlatten`'s
`CSwitch (VInt n)` reduction (committed 7048dde). The remaining switches are
only those where a continuation is passed as a function parameter and its tag
is not locally known — genuinely dynamic dispatch. For bell00 this leaves 3
switches of width 5; for output, 3 switches of width 7.

**Fix requires 0-CFA:** narrowing these sites requires tracking, across
declaration boundaries, which closure labels can flow to each continuation
parameter slot — the same cross-declaration propagation that
`ModuleRecordFlatten` does for record shapes, but for closure identity. A
whole-module fixed-point iteration over the call graph would be needed.

This is moderate effort and low urgency for current program sizes. Revisit
when example programs grow large enough that table width becomes measurably
costly.

---
