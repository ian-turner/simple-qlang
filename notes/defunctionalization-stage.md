# Defunctionalization Stage

Implemented a first defunctionalization pass in `src/Defunc.hs`.

## Current Strategy

- Closure conversion now emits synthetic code labels such as `_fun0`, `_fun1`,
  ... instead of relying on nominal variable pretty-printing
- Defunctionalization rewrites closure records so field 0 holds an integer tag
  rather than a code label
- The closure-conversion indirect-call pattern
  `SELECT(0, clo, tag, APP(tag, clo : args))`
  is rewritten to a `CSwitch` on `tag` with direct `VLabel` calls in each arm

## Why The Synthetic Labels Exist

Nominal variables do not print with unique names. Using `show Variable` for
code labels caused collisions like repeated `"d"` labels, which made
defunctionalization ambiguous. The closure-conversion pass now generates its
own unique internal labels for runtime-dispatch purposes.

## Current Limitation

The pass builds one dispatch table per declaration from all synthetic closure
labels appearing in that declaration. This is correct for the current pipeline,
but not minimal: many dispatch sites switch over more labels than they actually
need.

That is acceptable for now because:

- the goal of this stage is correctness and elimination of runtime code pointers
- later optimization passes can shrink dispatch sets or introduce shared apply
  functions if needed

## Follow-On Work

- tuple/record flattening will need to preserve the tag field produced here
- backend emission can now assume closure records contain plain data, not code
  pointers
- if top-level functions ever become first-class across declarations, this pass
  will need a module-level notion of defunctionalization tags rather than the
  current per-declaration scheme
