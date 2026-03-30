# Qubit Hoisting Stage

Implemented a first qubit-hoisting pass in `src/QubitHoist.hs`.

## Current Strategy

- The pass runs after defunctionalization and before any backend emission work
- Each `CPrimOp PInit [VUnit] [q] [cont]` is assigned the next static qubit
  slot in traversal order
- The bound result variable is replaced inside the continuation with
  `VQubit slot`
- The pass returns both the rewritten CPS expression and the total number of
  backend qubit slots required

## Why `VQubit` Exists

Using `VInt` for hoisted qubit slots would mix backend qubit identities with
ordinary classical integers. Adding `VQubit Int` to `CPSExp.Value` keeps the
IR explicit about the difference and gives later passes a stable way to
recognize preallocated qubit references.

## Current Limitation

The first version is intentionally conservative:

- one backend slot is reserved per `init`
- no liveness-based reuse is attempted
- the pass assumes the current CPS `PInit` shape and simply removes dynamic
  allocation from the IR

This matches the existing design notes and is enough to unblock tuple
flattening and backend emission.

## Follow-On Work

- tuple/record flattening must preserve `VQubit` leaves when flattening tuple
  records built from quantum primitives such as `cnot`
- OpenQASM emission should use `hoistedQubitCount` to declare `qubit q_i;`
  slots at program scope and translate `VQubit i` to the corresponding backend
  name
- a later optimisation pass can reuse slots once qubit lifetimes are tracked
