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

## Structural Changes Required

### 1. Add a module-level CPS pipeline representation

Instead of treating each top-level declaration as an isolated `CExp`, add a
representation for a compiled module after CPS conversion and middle-end
normalization.

Minimum useful shape:

- a list of named top-level declarations
- each declaration's CPS body after recursion elimination / closure conversion /
  defunctionalization / qubit hoisting
- enough metadata to identify entry declarations such as `output`

Possible direction:

- introduce a `CompiledDecl` / `CompiledModule` datatype
- move the post-lowering pipeline from `printLowered` in `src/Main.hs` into a
  module-oriented compilation function

### 2. Run record-shape analysis across the whole module

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

### 3. Rewrite function signatures and call sites together

Once record shapes are known, flattening must stop being a local substitution
pass and become a coordinated signature rewrite.

That rewrite needs to:

- replace record-valued parameters with multiple scalar parameters
- rewrite all corresponding call sites
- rewrite uses inside function bodies so former `SELECT`s become direct scalar
  references
- rewrite continuation-style calls the same way as ordinary function calls

This is the real missing piece for:

- tuple results from `PCNot`
- records passed through continuations
- cross-declaration calls such as `bell00` consumed by `output`

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

Refactor target:

- one pure compile pipeline that returns all intermediate artifacts needed
- one display/debug layer that prints those artifacts

That will make later passes easier to insert and easier to test.

## Suggested Near-Term Refactor Sequence

1. Extract per-declaration compilation from `src/Main.hs` into a pure helper.
2. Introduce a module-level datatype holding all compiled declarations.
3. Run existing middle-end passes into that datatype without changing behavior.
4. Add whole-module record-shape analysis.
5. Replace the current conservative `RecordFlatten` pass with a signature-aware
   flattening pass, or keep the local pass as a pre-pass and add a second
   interprocedural pass after it.

## Expected Payoff

After these changes, the compiler should be able to complete the actual
required record-flattening milestone:

- tuple records from quantum primitives no longer survive into backend emission
- `SELECT` chains over tuple data collapse into direct scalar variables
- OpenQASM emission can operate on scalar qubit/bit/int flows rather than CPS
  heap-record conventions

This is the structural prerequisite for the next two required stages:

- gate/`def` classification
- OpenQASM emission
