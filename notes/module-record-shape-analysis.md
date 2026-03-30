# Module Record Shape Analysis

This note records the first whole-module shape-analysis layer added after the
module compile-pipeline refactor.

## Current Strategy

- `src/RecordShape.hs` infers argument shapes for callable CPS functions across
  the whole module
- the analysis is run from `src/CompilePipeline.hs` and stored in
  `CompiledModule`
- the analysis currently operates on the post-recursion-check CPS, before
  closure conversion and defunctionalization

## Why It Runs Before Closure Conversion

That choice is deliberate.

At post-recursion CPS:

- function parameters and continuation parameters are still explicit `CFix`
  binders
- call sites are still direct `CApp (VVar f)` or cross-declaration
  `CApp (VLabel "name")`
- tuple/data-flow records are still visible without the extra closure records
  introduced later

This keeps the inferred shapes focused on the interfaces that matter for the
 record-flattening rewrite.

## Shape Lattice

The current analysis tracks four cases:

- `ShapeUnknown` for no information yet
- `ShapeScalar` for non-record values
- `ShapeRecord [...]` for a known flat record layout
- `ShapeOpaque` for conflicting or unsupported shapes

`ShapeOpaque` is intentionally conservative: if a parameter is observed with
 incompatible layouts, later rewriting should leave it alone.

## Current Limitation

This pass only infers function-interface shapes. It does not yet rewrite:

- function parameter lists
- `CApp` argument lists
- record projections inside function bodies

That rewrite is still the next step.
