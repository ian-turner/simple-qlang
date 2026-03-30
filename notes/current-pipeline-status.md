# Current Pipeline Status

This note records the implementation state that is already landed in the code
but is not consistently reflected in the top-level project docs.

## Landed Since The Original Stage Summary

The compiler no longer stops conceptually at qubit hoisting.

The current pipeline structure includes:

- module-level compilation orchestration in `src/CompilePipeline.hs`
- whole-module record-shape inference in `src/RecordShape.hs`
- a first interprocedural interface-flattening pass in `src/ModuleRecordFlatten.hs`
- a conservative local record-flattening pass in `src/RecordFlatten.hs`
- debug output for interface-flattened IR in `src/Main.hs`
- debug output for record-flattened IR in `src/Main.hs`

That means tuple/record flattening is no longer "not started". A first,
correctness-oriented subset is implemented and wired into the pipeline.

## What The Existing Record Flattening Actually Covers

The landed flattening work now has two layers.

The earlier module-level pass rewrites some function and continuation
interfaces before closure conversion when the inferred shape is fully known and
scalarizable.

The later local pass removes record traffic only when the record layout is
known within the current CPS expression.

In practice it already handles:

- some interprocedural tuple/data-flow interfaces in pre-closure CPS
- local tuple records introduced by CPS conversion
- immediate `SELECT`/`OFFSET` chains over locally known records
- records whose leaves include hoisted qubit values such as `VQubit`

This is useful preparation for backend work because it reduces administrative
record noise before emission exists.

## What The Current Record Flattening Now Covers

The continuation-result follow-on work is now landed as well.

Tuple/data-flow results can now flatten across top-level continuation
boundaries when shape inference can connect the producer and consumer
interfaces. That closes the original required record-flattening milestone for
ordinary tuple/data-flow traffic.

The remaining conservative boundary is intentional:

- interfaces inferred as `ShapeUnknown` or `ShapeOpaque` are left alone
- closure-conversion records are not aggressively flattened here
- defunctionalization records are also preserved for later backend-facing
  lowering if needed

## Practical Status Of The Backend

The compiler now has a first end-to-end OpenQASM backend path in
`src/OpenQASM.hs`.

That emitter currently:

- starts from `output`
- emits from the interface-flattened CPS
- performs global qubit allocation during emission
- inlines reachable top-level calls into one flat OpenQASM program
- refuses to run if earlier compilation stages rejected recursive top-level
  label cycles

Gate/`def` classification remains implemented as a conservative top-level pass
over the interface-flattened CPS in `src/GateDef.hs`.

Top-level recursion checking is now split across two places:

- `src/RecElim.hs` rejects recursive local `CFix` groups
- `src/CompilePipeline.hs` rejects recursive cycles between top-level
  declarations via `VLabel` call-graph analysis

## What Still Remains

The remaining work is now backend refinement rather than absence of a backend:

1. emit reusable OpenQASM `gate` / `def` declarations instead of only
   entrypoint-driven inlining
2. add bounded-recursion support through explicit loop lowering and static-list
   erasure rather than rejecting all finite recursive programs
3. decide the long-term backend boundary relative to closure conversion /
   defunctionalization
4. broaden emitter coverage and reduce duplicated code in generated output

The bounded-recursion design work now has a concrete sketch in
`notes/bounded-recursion-lowering-plan-2026-03-30.md`. The planned direction is
to add:

- a static shape/size inference pass after CPS conversion
- a bounded-recursion lowering pass that introduces explicit loop IR
- a static-list erasure pass before the later backend-facing stages

That work is required for examples such as `examples/ghz.funq`, whose recursive
helpers are finite but currently rejected because the compiler does not yet
lower fixed-size list recursion into loops and fixed aggregates.

## Documentation Drift To Keep In Mind

The main drift to watch for now is older handoff text that still says
continuation-result flattening, gate/`def` classification, or first OpenQASM
emission is the next step.

Those statements are stale. The current next backend step is emitter
refinement.
