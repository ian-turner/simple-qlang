# Record and Tuple Flattening

**Modules:** `src/RecordShape.hs`, `src/ModuleRecordFlatten.hs`, `src/CPSAtom.hs`, `src/RecordFlatten.hs`

See also: [../pipeline.md](../pipeline.md)

---

## Overview

OpenQASM has no tuple type. The CPS IR uses `CRecord`/`CSelect` for tuples
(e.g., qubit pairs from `cnot`). Flattening replaces each record with
individual scalar variables and rewrites all projections as direct variable
references.

Two passes run in sequence:
1. **Module-level interface flattening** (before closure conversion)
2. **Local record cleanup** (after qubit hoisting)

---

## Shape lattice (`RecordShape.hs`)

The analysis tracks four cases for each callable parameter slot:

| Shape | Meaning |
|---|---|
| `ShapeUnknown` | No information yet |
| `ShapeScalar` | Non-record value |
| `ShapeRecord [...]` | Known flat record layout |
| `ShapeOpaque` | Conflicting or unsupported layouts — leave alone |

`ShapeOpaque` is intentionally conservative: if a parameter is observed with
incompatible layouts across call sites, the rewriter leaves it alone.

The analysis runs on post-recursion CPS, **before** closure conversion, while:
- function parameters are still explicit `CFix` binders
- call sites are still direct `CApp (VVar f)` or cross-declaration `CApp (VLabel "name")`
- tuple/data-flow records are visible without the extra closure records added later

Shape keys include both `CFix`-bound names and `ParamFunction` slots for
continuation parameters, so tuple results flowing through continuation
boundaries can be flattened across declaration boundaries.

---

## Interface rewriting (`ModuleRecordFlatten.hs`)

For each callable whose parameter shape is a fully-scalar `ShapeRecord`, the pass:
- Replaces the record parameter with individual scalar parameters in the `CFix`
- Rewrites all matching `CApp` argument lists to pass the scalar leaves
- Seeds a local substitution so former `CSelect` projections become direct
  variable references

**Continuation-result flattening:** when a callable argument is passed into a
parameter slot whose call arity is already known, the callee-side continuation
interface shape is propagated back to the actual function supplied at the call
site. This lets tuple results flatten across top-level declaration boundaries
(e.g., `bell00`'s qubit pair result flowing into `output`'s continuation).

**Conservative rule:** only flatten shapes that are fully known and recursively
scalar. `ShapeUnknown`, `ShapeOpaque`, closure records, and defunctionalization
records are left alone.

---

## Local record cleanup (`RecordFlatten.hs`)

Runs after qubit hoisting. Tracks record layouts statically within one CPS
expression:
- Folds `CSelect`/`COffset` when the source record is locally known
- Drops `CRecord` bindings when all downstream uses have been eliminated
- Preserves `VQubit` leaves from the hoisting pass

This handles administrative tuple records introduced by CPS conversion that
stay within one declaration, and locally-constructed closure records whose
fields are immediately projected.

---

## What is intentionally not flattened

- Closure-conversion records — conservative by design; no aggressive flattening
- Defunctionalization records — preserved for backend-facing lowering
- Interfaces whose shape is `ShapeUnknown` or `ShapeOpaque`
- Interfaces inferred as partially-known nested records
