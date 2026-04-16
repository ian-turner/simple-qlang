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
