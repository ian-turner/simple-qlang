# Design Decisions

This file records project-level decisions that affect multiple compiler passes.

## Backend Scope

OpenQASM is the only active backend for now. The compilation pipeline should
remain target-neutral through the middle end so future backends such as QIR can
reuse the same normalized IR.

Implication:
- CPS conversion, recursion handling, closure conversion, and
  defunctionalization should not encode OpenQASM-specific constraints unless a
  backend-neutral formulation is impossible

## Measurement Semantics

`meas` is a consuming primitive.

- Surface signature: `Qubit -> Bool`
- Semantic rule: the input qubit is consumed by measurement and cannot be used
  afterward
- Enforcement point: the future linear type checker should reject any use of a
  measured qubit after the `meas` call

This keeps the surface language simple while making the linear lifetime rule
explicit in the specification.

## Initial Qubit Allocation Policy

The first backend allocation pass should use a simple static policy:

- count each `init` in the program
- allocate one backend qubit slot per `init`
- do not attempt liveness-based slot reuse yet

This is intentionally conservative. Reuse based on qubit liveness can be added
later as an optimization pass once correctness is established end-to-end.
