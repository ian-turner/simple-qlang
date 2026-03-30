# Module Compile Pipeline

This note records the structural refactor that moved middle-end orchestration
out of `src/Main.hs`.

## What Changed

- `src/CompilePipeline.hs` now owns the per-declaration compilation sequence
- that module returns a `CompiledModule` with a list of `CompiledItem`s
- successful definitions retain every currently visible artifact:
  - Lambda IR
  - CPS IR
  - post-recursion-check CPS
  - closure-converted IR
  - defunctionalized IR
  - qubit-hoisted IR
  - record-flattened IR
- `src/Main.hs` is now only responsible for:
  - parsing
  - scope resolution
  - invoking `compileModule`
  - printing the resulting artifacts

## Why This Matters

The previous structure compiled one declaration at a time inside the printing
path. That made it awkward to add module-level analyses and rewrites.

The new `CompiledModule` representation is the first step toward whole-module
record flattening:

- later passes can analyze all compiled declarations together
- entry-point metadata can live alongside the compiled declarations
- a signature-rewriting pass can be inserted without entangling it with debug
  output
- module-level record-shape analysis now runs against that representation

## Current Limitation

This refactor still does not perform interprocedural record flattening by
itself.

What still remains after the follow-on shape-analysis pass is:

- rewrite function parameters and `CApp` sites together
- rewrite affected function bodies so record `SELECT`s collapse to scalar names
- distinguish tuple/data-flow records from closure/defunctionalization records
