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

---

## Why `VQubit` exists

Using `VInt` for hoisted qubit slots would mix backend qubit identities with
ordinary classical integers. `VQubit Int` is a separate `Value` constructor in
`CPSExp.hs` that:
- Keeps the IR explicit about which values are qubit references
- Gives later passes (record flattening, gate/def classification, the emitter)
  a stable way to recognize preallocated qubit references
- Prevents qubit indices from being treated as arithmetic operands

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
