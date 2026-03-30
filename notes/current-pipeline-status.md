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

## What Still Blocks Completion

The remaining gap is full interprocedural flattening, especially across
top-level continuation boundaries.

Record-valued data-flow can still survive when it crosses:

- continuation parameters supplied from another declaration
- top-level function result paths encoded through CPS continuations
- any interface whose inferred shape remains `ShapeUnknown` or `ShapeOpaque`

The next implementation step is no longer "add interface flattening". It is to
extend shape propagation so continuation-parameter signatures connect across
declaration boundaries and let tuple-returning functions flatten all the way
into their consumers.

## Practical Status Of The Remaining OpenQASM Work

The compiler is beyond "qubit hoisting only", but it is not yet at the point
where a clean OpenQASM emitter can be added directly.

The remaining required milestones are:

1. finish interprocedural tuple/data-flow record flattening
2. classify closed functions as `gate` or `def`
3. emit OpenQASM from the flattened, closed CPS

## Documentation Drift To Keep In Mind

At the time of writing, some top-level docs still describe the compiler state
as:

- implemented through qubit hoisting only
- tuple/record flattening not started

Those statements are now stale. Treat the notes covering module compilation,
record-shape analysis, and record flattening as the more accurate source of
truth until the top-level summaries are refreshed.
