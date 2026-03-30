# Interface Record Flattening Stage

This note records the first interprocedural record-flattening pass added in
`src/ModuleRecordFlatten.hs`.

## What This Pass Does

The pass now runs on the post-recursion, pre-closure CPS for each compiled
declaration.

It uses `CompiledModule.compiledRecordShapes` to:

- rewrite `CFix` parameter lists when an inferred interface shape is a fully
  scalarizable `ShapeRecord`
- rewrite matching `CApp` argument lists to pass the flattened scalar leaves
- seed a local substitution environment so projections from flattened
  parameters collapse to direct scalar references
- preserve the existing local record simplification behavior while performing
  the interface rewrite

The later post-hoisting `RecordFlatten` pass still remains in place. The new
pass does not replace it; it moves the first coordinated interface rewrite
earlier in the pipeline.

## Conservative Rule

This pass flattens only shapes that are fully known and recursively scalar.

It does not flatten:

- `ShapeUnknown`
- `ShapeOpaque`
- partially known nested records

If the shape analysis is not confident, the original interface is preserved.

## Important Current Limitation

This first pass only rewrites interfaces that are directly represented in the
current `ModuleRecordShapes` map.

That leaves one important gap:

- record-valued results that flow through continuation parameters passed across
  function boundaries are not yet propagated interprocedurally

In the current CPS, a top-level function like `bell00` returns its tuple by
calling a continuation parameter. The present shape analysis tracks direct
callable entries (`CFix`-bound functions and top-level labels), but it does
not yet infer the interface shape of continuation parameters supplied from
another declaration.

So this stage is a correct first interface rewrite, but not the final
record-flattening milestone needed for backend emission.

## Follow-On Work

The next extension should connect continuation-parameter call signatures across
declaration boundaries so tuple results from functions like `bell00` can be
flattened all the way into consumers such as `output`.
