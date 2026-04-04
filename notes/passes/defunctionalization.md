# Defunctionalization

**Module:** `src/Defunc.hs`

See also: [../pipeline.md](../pipeline.md)

---

## Overview

OpenQASM has no function values. After closure conversion, closures are still
first-class data (code labels stored in records). Defunctionalization eliminates
these remaining function values by:

1. Replacing code-label fields in closure records with integer tags
2. Rewriting indirect closure calls into `CSwitch`-based dispatch over direct
   `VLabel` calls

---

## Strategy

Closure conversion generates synthetic code labels like `_fun0`, `_fun1`, ...
(unique, not relying on `show Variable` which produces non-unique names).

Defunctionalization:
- Builds a dispatch table from all synthetic closure labels appearing in each
  declaration
- Rewrites closure records so field 0 holds an integer tag instead of a code
  label
- Rewrites the closure-conversion indirect-call pattern
  `SELECT(0, clo, tag, APP(tag, clo : args))`
  into `SWITCH(tag, [direct VLabel calls])`

---

## Current limitation

The dispatch table is built per-declaration from all synthetic closure labels
in that declaration. This is correct but not minimal: many dispatch sites
switch over more labels than they actually need at that site.

This is acceptable for now — the goal is correctness and elimination of runtime
code pointers. Later optimization passes can shrink dispatch sets or introduce
shared `apply` functions if needed.

---

## Downstream implications

- Tuple/record flattening must preserve the integer tag field in dispatch records
- Backend emission can assume closure records contain plain data, not code pointers
- If top-level functions become first-class across declarations, a module-level
  defunctionalization tag scheme would be needed rather than the current
  per-declaration approach
