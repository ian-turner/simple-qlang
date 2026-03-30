# Record Flattening Next Step

This note is a short handoff for the next thread.

## What Is Already Landed

- local record simplification in `src/RecordFlatten.hs`
- module-level compilation in `src/CompilePipeline.hs`
- whole-module interface-shape inference in `src/RecordShape.hs`
- first interface rewrite pass in `src/ModuleRecordFlatten.hs`

## What Still Blocks Completion

The compiler now rewrites some record-valued function and continuation
interfaces, but it still does not flatten tuple/data-flow results that cross
top-level continuation boundaries.

That means tuple/data-flow records can still survive when they cross:

- continuation parameters supplied from another declaration
- top-level declaration boundaries via CPS return continuations
- any call edge whose interface shape is still inferred conservatively

## Immediate Implementation Target

Extend the shape-driven interface rewrite so it can follow continuation result
paths across declarations.

The next pass should:

- propagate shape information for continuation parameters that represent the
  result interface of another declaration
- let top-level producers such as `bell00` and top-level consumers such as
  `output` agree on flattened continuation signatures
- preserve the current conservative rule for `ShapeUnknown` and `ShapeOpaque`
- keep the existing `ModuleRecordFlatten` rewrite as the execution point for
  the actual signature transformation

## Conservative Rule

Prefer not flattening to flattening the wrong thing.

Continue flattening only interfaces whose shape clearly comes from
tuple/data-flow records. Closure/defunctionalization records can remain
conservative until the backend needs a more explicit classification pass.
