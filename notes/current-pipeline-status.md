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

## Practical Status Of The Remaining OpenQASM Work

The compiler is beyond "qubit hoisting only", but it is not yet at the point
where a clean OpenQASM emitter can be added directly.

The remaining required milestone is:

1. emit OpenQASM from the flattened, closed CPS

Gate/`def` classification is now implemented as a conservative top-level pass
over the interface-flattened CPS in `src/GateDef.hs`. That gives the compiler a
stable classification summary without making later closure/dispatch scaffolding
drive every function to `def`.

## Documentation Drift To Keep In Mind

The main drift to watch for now is older handoff text that still says
continuation-result flattening or gate/`def` classification is the next step.

Those statements are stale. The current next required backend step is OpenQASM
emission.
