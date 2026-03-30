# Module-Level Record Flattening Plan

This note records the structural changes needed to complete the required
tuple/record-flattening stage.

## Why The Current Pass Is Incomplete

`src/RecordFlatten.hs` only simplifies records whose layout is known locally
inside one CPS expression. That is enough to remove some administrative record
traffic, but it cannot eliminate records that cross function boundaries.

The main reason is that the current driver in `src/Main.hs` compiles and prints
each top-level declaration independently:

- lower one declaration
- CPS-convert one declaration
- run the middle-end passes on that declaration alone

That structure prevents a later pass from rewriting function signatures and
call sites consistently across a whole module.

## Structural Progress

The first two structural prerequisites from this note are now implemented:

- module-level compilation now lives in `src/CompilePipeline.hs`
- whole-module record-shape analysis now lives in `src/RecordShape.hs`

That means the remaining work is no longer architectural setup.

Status update:

- the first signature-aware flattening rewrite now exists in
  `src/ModuleRecordFlatten.hs`
- `src/CompilePipeline.hs` now runs that pass before closure conversion
- `src/Main.hs` now prints an `Interface-Flattened IR` stage
- continuation-result flattening across top-level declaration boundaries is now
  also landed
- gate/`def` classification now runs after interface flattening

That means this note is now mostly historical: the structural and analysis
prerequisites it called for are in place.

## Remaining Structural Changes

### 1. Module-level CPS pipeline representation

Instead of treating each top-level declaration as an isolated `CExp`, add a
representation for a compiled module after CPS conversion and middle-end
normalization.

Minimum useful shape:

- a list of named top-level declarations
- each declaration's CPS body after recursion elimination / closure conversion /
  defunctionalization / qubit hoisting
- enough metadata to identify entry declarations such as `output`

Status:

- implemented as `CompiledDecl` / `CompiledModule` in `src/CompilePipeline.hs`
- `src/Main.hs` now formats compiled artifacts instead of driving the pipeline

### 2. Whole-module record-shape analysis

The compiler needs a pass that tracks which variables and function parameters
have scalar shape vs record shape.

This analysis must understand:

- records created by `CRecord`
- projections via `CSelect` and `COffset`
- function arguments at `CApp`
- function parameters in `CFix`
- record-valued results passed through continuation calls

The output should be a stable notion of function-interface shape, for example:

- parameter 0 of function `f` is scalar
- parameter 1 of function `f` is a 2-field record of scalars
- function `k` is always called with a 3-field record in argument slot 1

Status:

- implemented conservatively in `src/RecordShape.hs`
- currently stored on `CompiledModule`
- currently printed as a top-level debug summary in `src/Main.hs`

### 3. Rewrite function signatures and call sites together

Once record shapes are known, flattening must stop being a local substitution
pass and become a coordinated signature rewrite.

That rewrite needs to:

- replace record-valued parameters with multiple scalar parameters
- rewrite all corresponding call sites
- rewrite uses inside function bodies so former `SELECT`s become direct scalar
  references
- rewrite continuation-style calls the same way as ordinary function calls

This was the real missing piece for:

- tuple results from `PCNot`
- records passed through continuations
- cross-declaration calls such as `bell00` consumed by `output`

Status:

- partially implemented in `src/ModuleRecordFlatten.hs`
- verified to rewrite some local continuation interfaces and `PCNot`
  tuple handoffs
- later completed for cross-declaration continuation-result flow; see
  `notes/continuation-result-record-flattening.md`

### 4. Separate tuple flattening from closure-record handling

The current CPS uses `CRecord` for multiple purposes:

- tuple values
- closure/environment records
- defunctionalized tagged records

The full pass should avoid blindly flattening all of them in the same way.

Recommended approach:

- flatten tuple-like records that are part of ordinary data flow
- preserve closure/defunctionalization records until a later backend-facing
  lowering step, unless their fields are only consumed locally and can be
  folded away safely

This likely requires a small notion of record provenance or at least a
conservative classification heuristic.

### 5. Move pipeline orchestration out of printing code

`src/Main.hs` currently mixes:

- compilation logic
- stage sequencing
- pretty-print/debug output

That makes it hard to introduce module-level analyses.

Status:

- implemented by the `CompilePipeline` refactor
- later passes can now be inserted without changing `Main`'s compilation logic

## Historical Follow-On

The near-term refactor sequence and handoff work described here have now been
completed:

1. `ModuleRecordShapes` identifies flattenable tuple/data-flow interfaces.
2. `ModuleRecordFlatten` rewrites matching `CFix` and `CApp` interfaces.
3. Flattened parameter projections collapse to direct scalar variables.
4. The local `RecordFlatten` pass remains as a later cleanup pass.
5. Closure/defunctionalization records stay conservative.

For the current next-stage handoff, see:

- `notes/gate-def-classification-stage.md`
- `notes/current-pipeline-status.md`

## Expected Payoff

After this follow-on work, the compiler should be able to complete the actual
required record-flattening milestone:

- tuple records from quantum primitives no longer survive into backend emission
- `SELECT` chains over tuple data collapse into direct scalar variables
- OpenQASM emission can operate on scalar qubit/bit/int flows rather than CPS
  heap-record conventions

This structural work is now the prerequisite that remains in place for the
final required backend stage:

- OpenQASM emission
