# Inlined If/Else Suffix Hoisting

This note records a backend-only refinement to OpenQASM emission for dynamic
two-arm branches.

## What changed

`src/OpenQASM.hs` now compares the emitted statement lists for the `false` and
`true` arms of a dynamic two-arm `CSwitch`.

If both arms end with an identical trailing statement suffix, the emitter:

- keeps only the differing prefixes inside the generated OpenQASM `if/else`
- emits the shared suffix once after the branch
- collapses to a single-arm `if` when one branch prefix becomes empty

That means patterns such as classically controlled gate application can now
emit as:

```qasm
if (b == 1) {
  x q[0];
}
...shared continuation...
```

instead of duplicating the shared continuation inside both arms.

## Why this is limited

This is still a one-pass emitter optimization, not a general join-point
implementation.

It only hoists statement sequences that are textually identical at the end of
both emitted arms. It does not yet:

- introduce reusable OpenQASM `def` helpers for shared continuations
- create backend phi-like merges for branch-produced values

So it improves common inline control cases, but it is not the final answer for
arbitrary branch merging.

The CPS-level fix (named join continuations in `ToCPS.hs`) was landed
separately — see `notes/switch-join-continuation-2026-03-31.md`. The emitter
still uses suffix hoisting to deduplicate the OpenQASM output it produces from
those join-continuation calls.
