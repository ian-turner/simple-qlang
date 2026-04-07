# Qubit Hoisting

**Module:** `src/QubitHoist.hs`

See also: [../pipeline.md](../pipeline.md), [../quantum-semantics.md](../quantum-semantics.md)

---

## Overview

OpenQASM requires all qubit declarations at top-level scope. FunQ's `init()`
creates qubits dynamically inside expressions. Qubit hoisting:

1. Traverses the CPS expression in order, assigning each `PRIMOP(init, ...)` the
   next static slot index
2. Replaces each bound result variable with `VQubit slot`
3. Returns the total number of required qubit slots alongside the rewritten CPS

`hoistQubits` currently rewrites only ordinary `PRIMOP(init, ...)` sites into
`VQubit`. Its `CFor` handling is structural traversal only; the planned
`CFor`-specific batch allocation path is not implemented yet.

---

## Why `VQubit` and `VQubitArr` exist

Using `VInt` for hoisted qubit slots would mix backend qubit identities with
ordinary classical integers. `CPSExp.hs` therefore defines dedicated qubit
reference forms:

- `VQubit Int` for today's statically hoisted single-slot qubits
- `VQubitArr Int Value` for the planned counted-loop path, where a statically
  allocated batch is indexed by a loop variable

These constructors:
- keep the IR explicit about which values are qubit references
- give later passes (record flattening, gate/def classification, the emitter)
  a stable way to recognize preallocated qubit references
- prevent qubit indices from being treated as arithmetic operands

---

## Current policy: one slot per `init`

The first version is intentionally conservative:
- One backend slot reserved per `init` in the program
- No liveness-based reuse

This is enough to unblock tuple flattening and backend emission. Liveness-based
slot reuse can be added later as an optimization once qubit lifetimes are
tracked precisely.

---

## Downstream implications

- Record flattening must preserve `VQubit` leaves when flattening tuple records
  built from multi-output quantum primitives like `cnot`
- The OpenQASM emitter uses the returned `hoistedQubitCount` to declare
  `qubit q[n];` at program scope, and translates `VQubit i` to `q[i]`
- `VQubitArr` is reserved for the planned `CFor` path; `QubitHoist.hs` can
  rewrite inside it structurally, but the current emitter does not consume it
  end-to-end because `CFor` lowering is not yet wired
