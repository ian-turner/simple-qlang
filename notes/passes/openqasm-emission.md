# OpenQASM Emission

**Modules:** `src/OpenQASM.hs` (emitter), `src/QASMAnalysis.hs` (CPS analysis), `src/QASMRender.hs` (rendering)

See also: [../pipeline.md](../pipeline.md), [../future/backend-refactor.md](../future/backend-refactor.md)

---

## Architecture

The emitter is entrypoint-driven and interpretive:
- Starts from the `output` declaration
- Inlines top-level calls reachable from `output` via `VLabel` lookup
- Emits one flat OpenQASM 3.0 program
- Runs on the interface-flattened CPS (not the fully defunctionalized form)

### Why emit from interface-flattened CPS

The closure-converted and defunctionalized forms are correct but introduce
large amounts of closure/tag scaffolding. The interface-flattened CPS is a
better backend boundary because:
- Tuple/data-flow records are already flattened across declaration boundaries
- Top-level calls are still direct `VLabel` calls
- Continuation structure is explicit without closure records
- Global qubit allocation can be performed during emission rather than per-declaration

---

## CPS → OpenQASM translation table

| CPS construct | OpenQASM output |
|---|---|
| `PRIMOP(init, [], [q])` | Uses `qubit q[n];` declared at top scope |
| `PRIMOP(hgate, [q], [q'])` | `h q[i];` |
| `PRIMOP(xgate, [q], [q'])` | `x q[i];` |
| `PRIMOP(zgate, [q], [q'])` | `z q[i];` |
| `PRIMOP(cnot, [q1,q2], [q1',q2'])` | `cx q[i], q[j];` |
| `PRIMOP(meas, [q], [c], [E])` | `bit c = measure q[i];` then `E` |
| `CSwitch(v, [E0, E1])` | `if (v == 1) { E1 } else { E0 }` |
| `CSwitch(v, [E0, E1, ...])` | `switch (v) { case 0: E0; ... }` |
| Self-recursive tail loop | `while (cond) { body }` |
| Classical `PRIMOP(+, ...)` | inline arithmetic expression |

---

## Output declaration layout

After flattening the final classical result leaves, the emitter chooses a
layout:

- **Homogeneous `bit` outputs** (≥ 2 leaves): `bit[n] output;`
- **Homogeneous non-bit outputs** (≥ 2 leaves): `array[type, n] output;`
- **Mixed-type outputs**: individual `output_i` declarations (OpenQASM arrays
  are homogeneous)

---

## Float constant preservation

Float literals (including `pi`) are stored as `String` from parsing through to
emission. The emitter renders them directly into OpenQASM expressions:
```openqasm
u(pi, 0, 0) q[0];
```

This keeps the middle end backend-neutral — a future QIR backend can lower
symbolic constants differently without changing the front half of the compiler.

---

## Suffix hoisting for branch deduplication

When a two-arm `CSwitch` is emitted, the emitter compares the trailing
statement suffixes of both arms. If they are textually identical, the shared
suffix is hoisted out of the `if/else` and emitted once afterward:

```openqasm
if (b == 1) {
  x q[0];    // differing prefix only
}
// shared continuation here
```

This is a one-pass heuristic, not a full join-point implementation. It only
hoists textually identical trailing sequences. The CPS-level fix (named join
continuations in `ToCPS.hs`) prevents duplication in the IR; suffix hoisting
deduplicates the emitter's output.

The longer-term improvement — teaching the emitter to recognize join-
continuation calls as branch exits and run the continuation code once — is
tracked in [../future/backend-refactor.md](../future/backend-refactor.md).

---

## Tail-loop compilation (`isTailLoop`)

`isTailLoop` tests whether a self-recursive declaration is a tail loop: every
recursive call passes the outer continuation unchanged (directly or via
η-trivial chains). If so, the function emits as an OpenQASM `while` loop.

This recognition lives in `QASMAnalysis.hs` (`isTailLoop`), which `OpenQASM.hs`
imports. It is not yet a separate earlier normalization pass. It assumes the well-formed continuation shapes produced by
`ToCPS.hs`.

Otherwise the emitter falls back to guarded inline expansion with a 1000-call
depth limit. A compile-time error is raised if the limit is reached.

See [recursion.md](recursion.md) for the full recursion strategy.

---

## Deliberate limitations

This is a first-cut emitter, not the final backend architecture:

- Does not emit reusable OpenQASM `gate`/`def` declarations — helpers are
  always inlined from `output`. Repeated helper use duplicates code.
- Callable kinds are computed earlier for analysis/debugging, but the emitter
  does not yet consume them to choose between reusable OpenQASM `gate` and
  `def` declarations.
- Bounded list recursion is still handled by budget-unrolling rather than
  explicit loop IR. See [../future/bounded-recursion.md](../future/bounded-recursion.md).
- `CFor` and `VQubitArr` exist in the IR as scaffolding for planned bounded-
  recursion lowering. If a `CFor` node reaches `runExp` today, emission fails
  immediately with an unsupported-feature error.
- The `output` continuation is intercepted directly rather than relying on the
  final tiny local halt wrapper.
