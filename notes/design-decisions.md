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

## Dynamic lifting is implicit; no `bit` type

Unlike Proto-Quipper, FunQ has no explicit `dynlift` operation and no separate
`bit` type. Measurement (`meas`) produces a `Bool` directly — dynamic lifting
is considered implicit in the measurement semantics. This simplifies the surface
language and the type system by collapsing `Bit` and `Bool` into a single
classical boolean type.

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

## Separate recursion normalization from backend join lowering

Recent paper reading clarified that the remaining recursion work and the
`if/else` join cleanup are related but should not be treated as one refactor.

- **Bounded recursion** is primarily an upstream normalization/classification
  problem: expose loop-carried state, recover static bounds, then classify each
  recursive program as compile-time evaluation, static `for`, dynamic `while`,
  or rejection.
- **Branch joining** is primarily an emitter/control-flow problem: lower named
  local continuations as backend joins/blocks instead of recovering shared tails
  with suffix hoisting.

This means recursion lowering does **not** depend on first building a CFG/SSA
backend. A more explicit join/block backend may make later control lowering
cleaner, but it does not replace the need for:
- static shape inference
- early rejection of dynamic qubit-growing recursion
- TRMC/TMC-style recognition of residual recursive context
- explicit `CFor` lowering for statically bounded recursion

Conversely, explicit join handling should be landed as a focused emitter change
without waiting for bounded-recursion work. The current CPS IR already contains
the right `LSwitch` join-continuation shape.
