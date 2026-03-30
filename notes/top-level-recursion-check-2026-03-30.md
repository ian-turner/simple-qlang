# Top-Level Recursion Check

The OpenQASM emitter inlines reachable top-level labels by evaluating
`VLabel` references out of the compiled module environment. Local `CFix`
recursion was already rejected by `RecElim`, but top-level recursive
declarations were compiled independently and could still form label cycles such
as `init_n -> init_n` or mutual recursion across declarations.

That gap mattered for examples like `examples/ghz.funq`: the backend would try
to inline the recursive top-level declaration instead of rejecting it before
emission. The emitter has an internal "in progress" guard, but recursion should
be treated as a compile-time pipeline error, not as a backend-time failure mode.

`src/CompilePipeline.hs` now performs a module-level recursion check after
per-declaration CPS lowering and local recursion checking:

- it collects top-level `VLabel` callees from each declaration's CPS
- it builds a call graph over compiled declarations
- it marks every declaration in a cyclic SCC as a recursion error

This keeps recursive label cycles out of interface flattening, callable
classification, and OpenQASM emission.
