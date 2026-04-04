# Design Decisions

Project-level decisions that affect multiple compiler passes.

See also: [quantum-semantics.md](quantum-semantics.md), [pipeline.md](pipeline.md)

---

## Backend scope

OpenQASM is the only active backend. The pipeline stays target-neutral through
the middle end so a future QIR backend can reuse the same normalized IR.

- CPS conversion, recursion handling, closure conversion, and
  defunctionalization must not encode OpenQASM-specific constraints unless a
  backend-neutral formulation is impossible
- OpenQASM-specific passes begin with qubit declaration strategy, gate/def
  classification, and final emission

## Measurement is a single-result primitive

`meas : Qubit -> Bool`. The input qubit is consumed; any branching on the
result goes through a separate `CSwitch`. See
[quantum-semantics.md](quantum-semantics.md) for rationale and the full
constraint context.

## Emit from interface-flattened CPS, not fully defunctionalized form

The current emitter runs on the interface-flattened CPS rather than the
closure-converted/defunctionalized output. This boundary:
- Keeps top-level calls as direct `VLabel` references (readable, debuggable)
- Avoids closure/tag scaffolding that is correct but irrelevant to OpenQASM
  emission
- Lets global qubit allocation happen during emission rather than per-declaration

When a more sophisticated backend is needed (reusable `gate`/`def` declarations,
explicit join-point lowering), the boundary will move later in the pipeline.

## Qubit allocation: one slot per `init`

Conservative static policy: one backend qubit slot per `init` in the program,
no liveness-based reuse. Slot reuse can be added later as an optimization once
correctness is established end-to-end.

See [passes/qubit-hoisting.md](passes/qubit-hoisting.md).

## Float literals are preserved symbolically

Float literals (including `pi`) are stored as `String` from parsing through to
emission. This keeps the middle end backend-neutral — OpenQASM can use `pi`
directly, and a future QIR backend can lower symbolic constants differently.

## Gate/def classification runs on interface-flattened CPS

The classifier runs before closure conversion so it sees clean pre-closure
interfaces. After closure conversion, nearly everything has extra records and
indirect calls that would collapse the `gate` class.

See [passes/gate-def-classification.md](passes/gate-def-classification.md).
