# OpenQASM Emission Stage

This note records the first landed OpenQASM backend path.

## What Changed

- `src/OpenQASM.hs` adds a first emitter that lowers the
  interface-flattened CPS directly to OpenQASM 3 text.
- `src/Main.hs` now prints an `OpenQASM` section after the existing IR and
  analysis summaries.
- The emitter runs from the module pipeline output but intentionally uses the
  `compiledInterfaceIR` stage rather than the closure-converted /
  defunctionalized forms.

## Why It Emits From Interface-Flattened CPS

The current closure-converted and defunctionalized forms are correct middle-end
artifacts, but they introduce a large amount of closure/tag scaffolding that is
not useful for a first OpenQASM backend.

The interface-flattened CPS is a better first backend boundary because:

- tuple/data-flow records are already flattened across declaration boundaries
- top-level calls are still direct `VLabel` calls
- continuation structure is explicit without closure records
- qubit allocation can be performed globally during emission instead of per
  declaration

## Current Backend Shape

The first emitter is intentionally entrypoint-driven.

- It starts from the `output` declaration.
- It inlines top-level calls reachable from `output`.
- It emits one flat OpenQASM program with top-level `qubit[...]` declarations,
  intermediate measurement bits, and `output_i` bits.

This gets the compiler to the first end-to-end generated OpenQASM program for
the current examples without yet committing to the final reusable `gate` /
`def` declaration strategy.

## Supported Cases

The landed emitter currently handles:

- `init`
- `hgate`, `xgate`, `zgate`, `cnot`
- `meas`
- CPS continuations and direct top-level calls
- flattened tuple/data-flow records
- simple classical `switch`
- primitive function values such as `xgate` and `zgate` passed through
  top-level higher-order code

Validated with:

- `cabal build`
- `cabal run funq -- examples/bell00.funq`
- `cabal run funq -- examples/tele.funq`
- `cabal run funq -- examples/ghz.funq` (produces correct 4-qubit GHZ circuit)

## Deliberate Limitations

This is not yet the final backend architecture.

- It does not emit reusable OpenQASM `gate` / `def` declarations yet.
- It emits by inlining from `output`, so repeated helper use will duplicate
  code in the generated QASM.
- It assumes the root `output` continuation is the backend result boundary and
  intercepts that directly rather than relying on the final tiny local halt
  wrapper.
- It implements bounded recursion via budget-unrolling for the subset recognized
  by `src/BoundedRecursion.hs`. General bounded recursion and OpenQASM `for`
  lowering remain future work.

The next backend improvement should be to factor the emitter so the current
entrypoint-linearization logic can be reused while introducing explicit
top-level `gate` / `def` emission based on `src/GateDef.hs`.
