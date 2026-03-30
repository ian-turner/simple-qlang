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
  - interface-flattened IR
  - gate/`def` classification
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

The new `CompiledModule` representation is what enabled whole-module
record-shape analysis, interface flattening, and callable classification:

- later passes can analyze all compiled declarations together
- entry-point metadata can live alongside the compiled declarations
- a signature-rewriting pass can be inserted without entangling it with debug
  output
- module-level record-shape analysis now runs against that representation
- gate/`def` classification now also runs against that representation

## Current Limitation

The refactor itself was infrastructure for later passes. Those follow-on passes
are now landed, and the backend no longer stops before emission.

Current backend limitations are refinement issues rather than missing
infrastructure:

- reusable OpenQASM `gate` / `def` emission is still not implemented
- bounded recursion still relies on budget-unrolling rather than explicit loop
  lowering
- generated control flow now hoists identical trailing suffixes out of dynamic
  two-arm branches, but general join-point extraction is still not implemented
