# Record Flattening Next Step

This note is a short handoff for the next thread.

## What Is Already Landed

- local record simplification in `src/RecordFlatten.hs`
- module-level compilation in `src/CompilePipeline.hs`
- whole-module interface-shape inference in `src/RecordShape.hs`

## What Still Blocks Completion

The compiler still does not rewrite record-valued function interfaces.

That means tuple/data-flow records can still survive when they cross:

- local `CFix` function boundaries
- continuation boundaries
- top-level declaration boundaries

## Immediate Implementation Target

Add a signature-aware flattening pass that operates on the pre-closure CPS
stored in `CompiledDecl.compiledRecursionResult`.

The pass should:

- read inferred shapes from `CompiledModule.compiledRecordShapes`
- rewrite `CFix` parameter lists when a parameter shape is `ShapeRecord [...]`
- rewrite matching `CApp` argument lists in the same pass
- rewrite uses inside affected bodies so field projections over flattened
  parameters become direct scalar references
- leave `ShapeOpaque` interfaces untouched

## Conservative Rule

Prefer not flattening to flattening the wrong thing.

For now, flatten only interfaces whose shape clearly comes from tuple/data-flow
records. Closure/defunctionalization records can remain conservative until the
backend needs a more explicit classification pass.
