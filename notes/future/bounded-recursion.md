# Bounded Recursion Lowering — Future Plan

See also: [../passes/recursion.md](../passes/recursion.md), [../pipeline.md](../pipeline.md), [../resources/leijen-lorenzen-trmc.md](../resources/leijen-lorenzen-trmc.md), [../resources/ohori-sasano-fixed-point-promotion.md](../resources/ohori-sasano-fixed-point-promotion.md)

---

## Current state

Self-recursive tail loops compile to OpenQASM `while` loops via `isTailLoop`.
All other recursive programs — including structural list recursion — are
handled by budget-unrolling in the emitter (depth limit: 1000 calls). This
covers current examples like `ghz.funq` and `qft_n.funq` but is fragile because
recursive ADT semantics are split across CPS lowering, interface flattening, and
emitter-side `ValueRep` reconstruction.

Scaffolding for the planned replacement already exists in the codebase:
`CFor`, `VQubitArr`, structural traversal cases in downstream passes, and
`StaticShape.hs`. What is still missing is end-to-end pipeline wiring,
`CFor`-producing lowering, and emitter support.

**Recent progress:** the old Class 3 correctness gap is closed. `OpenQASM.hs`
now rejects the dynamic qubit-growing case (for example `init_n x` where `x` is
a runtime variable) before inline expansion. The remaining work is structural:
wire shape analysis into the pipeline, lower bounded recursion to `CFor`, erase
static lists, and add real loop-aware hoisting and emission.

---

## Refined direction

The intended end state is stricter than the current implementation:
- Residual recursion must never reach emission.
- Every recursive program must be either:
  - evaluated away at compile time,
  - lowered to an explicit loop form, or
  - rejected with a compile-time error.
- Budget-unrolling in the emitter is transitional only.

This yields four practical buckets:

### 0. Static classical recursion → evaluate away

If a self-recursive function is pure classical code and all arguments relevant
to the recursion are compile-time constants, it should be partially evaluated
before backend emission rather than turned into a loop.

Example: `countdown_zero 3` should reduce to `True`; emitting a runtime `while`
loop is correct but unnecessarily low-level. More generally, classical helper
recursion that only computes constants, indices, loop bounds, or dead control
flow should disappear during compilation.

This bucket also includes recursive producer/consumer pairs whose intermediate
aggregate can be removed by upstream normalization such as fixed-point
promotion. That is still a "simplify it away before loop lowering" move, not a
backend control-flow transformation.

This is not a backend concern. It should be handled by a dedicated classical
partial-evaluation / constant-propagation pass that runs before bounded
recursion lowering.

### 1. Static bounded recursion → explicit `for`

If recursion describes finite traversal of statically sized data or a statically
bounded counter, lower it to explicit loop IR rather than unrolling it.

This includes:
- integer countdown/build loops like `init_n`
- structural list recursion over statically known lists like `meas_all`
- loop-carried traversals like `cnot_layer`
- nested traversals like `qft_core` / `apply_rotations`

The important point is that `ghz` and `qft_n` are not special cases that need
recursive emitter evaluation. They are ordinary static traversals once list
sizes and loop-carried state are made explicit.

### 2. Dynamic qubit-neutral recursion → explicit `while`

If the trip count is not statically known but the recursive body is
qubit-neutral, lower to an explicit dynamic loop. `rus_loop` remains the
canonical example.

Today this is recognized late by `isTailLoop` inside `QASMAnalysis.hs` (imported by `OpenQASM.hs`). That is
good enough as a first cut, but the long-term target should be an earlier IR
representation for dynamic loops rather than backend-only pattern matching.

### 3. Everything else → reject

Any recursive shape that is not statically evaluable, not statically bounded,
and not a qubit-neutral dynamic loop must fail before emission.

In particular, dynamic qubit-allocating recursion such as `init_n x` must be
rejected; there is no meaningful OpenQASM lowering because the total qubit
count is not statically bounded.

---

## Reading-based refinement

Recent reading narrowed the role of each paper in this work:

- **TRMC / TMC** are the main references for recognizing structural recursion
  whose recursive calls sit under residual constructor or algebraic context.
  They help expose loop-carried state; they are not the final backend
  representation.
- **SpecConstr** suggests a small upstream specialization step for recursive
  helpers whose state is obscured by known constructor shapes. This is best
  treated as a simplification pass before legality classification, not as a
  replacement for shape inference.
- **Fixed-point promotion** suggests a conservative producer/consumer fusion
  step for classical recursive plumbing before CPS conversion, again as an
  upstream simplifier rather than a loop-lowering mechanism.

The resulting picture is:
- simplify recursive state first when it is obviously removable
- expose residual post-recursive work as explicit carried state
- recover a static iteration space where possible
- then lower to `CFor`, `while`, or a compile-time error

See [../design-decisions.md](../design-decisions.md) and
[backend-refactor.md](backend-refactor.md) for the project-level split:
bounded recursion is blocked on recovering bounded iteration and legality, not
on first building a CFG / explicit-join backend.

---

## Architecture decision: `CFor` IR node, not unrolling

The chosen approach is to add a `CFor` constructor to `CPSExp.hs` and thread it
through all downstream passes, rather than eagerly unrolling static loops in the
middle end. Reasons:

- Unrolling is cheaper to implement but bloats the IR and the emitted OpenQASM
  for any moderately large N, and makes it impossible to emit a compact
  OpenQASM `for` loop.
- `CFor` with a statically-known trip count emits as a single `for i in [0:N]`
  statement in OpenQASM 3.0, keeping the output readable and bounded in size.
- The architectural cost is updating ClosureConv, Defunc, QubitHoist,
  RecordFlatten, and the OpenQASM emitter to handle `CFor` and the companion
  `VQubitArr` value constructor. These are mostly mechanical traversal cases,
  with the exception of QubitHoist (see below).

This still looks like the right first implementation step. The backend-refactor
work in [backend-refactor.md](backend-refactor.md) is a downstream emitter
cleanup, not the dependency edge for this note. `CFor` is still needed because
bounded-recursion lowering must solve:
- static classical evaluation,
- shape inference for list sizes,
- structural recursion recognition, and
- early rejection of unsupported recursive programs.

Leijen and Lorenzen's TRMC paper is useful here as a **recognition** reference:
it shows how recursive calls under constructors or algebraic residual contexts
can be normalized into tail-recursive workers with explicit carried state.
However, it is not a substitute for `CFor` in FunQ. Tail-recursive workers still
hide output size in an accumulator/context unless we recover a static bound and
erase the aggregate shape before emission. For FunQ, TRMC is upstream guidance
for exposing loop-carried state, not the final backend representation.

`CFor` represents Class 1 (static) loops only. The upper bound must be
statically derivable from the top-level call site — either a literal `VInt n`
or a `VVar` that refers to an outer loop index (for nested loops). The lower
bound may be a `VVar` from an outer loop (e.g. `i+1` for the inner loop of a
QFT). Class 2 (dynamic) loops continue to use the existing `isTailLoop` →
`while` path. There is no unbounded-trip-count `CFor`.

---

## Goal

Support recursive functions that can be rewritten into finite loops or
fixed-size dataflow before backend emission, without attempting general
recursion. The key constraint driving all decisions is that OpenQASM qubit
arrays must have a statically-known size.

This induces a two-class design for self-recursive functions:

- **Class 1 — static (`for` loop)**: trip count is a compile-time constant;
  qubits may accumulate across iterations because the total is statically
  bounded. The counter argument at the call site must be a `VInt n` literal
  (or evaluable by constant folding). Target: OpenQASM `for i in [0:N]`.

- **Class 2 — dynamic (`while` loop)**: trip count is not statically known;
  requires a **qubit-neutral body** — every `init` in the body is consumed by
  a `meas` on that same qubit before the recursive call. No new qubits escape
  to the next iteration; the qubit parameter set is preserved exactly. QubitHoist
  assigns one slot per `init` inside the body; physical reuse is valid because
  measurement collapses the qubit to `|0>`. Target: OpenQASM `while (cond)`.

- **Class 3 — reject**: dynamic trip count and qubit accumulation. Must fail
  early with an error explaining that the trip count must be a compile-time
  constant when the function allocates qubits proportional to the count.

**Type-level encoding:** with linear types, the class follows from `qubits(B)` vs
`qubits(A)` for a function `A → B` (where `qubits(T)` counts qubit slots in `T`,
with `List a` of length n contributing `n × qubits(a)`). If `qubits(B) > qubits(A)`,
the function must be Class 1. If `qubits(B) = qubits(A)`, it is a Class 2 candidate.

Target patterns:
- Counted self-recursion over `Int` with static argument → Class 1 (`for`)
- Structural recursion over statically sized lists → Class 1 after shape inference
- Qubit-neutral tail loops (like RUS) → Class 2 (`while`)

Anything outside that fragment should produce a compile-time error.

---

## Proposed new passes

### 0. Class 3 early rejection (`src/OpenQASM.hs`) — done

The emitter now rejects the case where a recursive function contains
`CPrimOp PInit` in its body and is called without any compile-time constant
integer argument, instead of falling into inline expansion at depth 1000.

Detection remains emitter-side: at the call site `CApp (VLabel name) (arg : _)`,
if `name` is known to be recursive and qubit-allocating and no argument is a
static `VInt n`, emission fails with a compile-time error explaining that the
qubit-growing recursion requires a compile-time bound.

Note: Class 2 qubit-neutrality (the `while`-loop case) is already enforced
implicitly by `isTailLoop`. A function that modifies its continuation — which is
the only way to accumulate qubits across iterations — already fails `isTailLoop`
and never reaches the while-loop path. No additional while-loop check is needed
under the linear-typing assumption.

### 0.5. Optional upstream normalization on `LambdaIR`

Before bounded-recursion lowering proper, the most promising new reading-driven
extensions are small direct-style normalization passes:

- **Shape-driven recursive specialization** for first-order self-recursive
  helpers whose recursive arguments are already known constructor forms and are
  immediately scrutinized again.
- **Recursive producer/consumer fusion** for visible local classical functions
  where a recursive producer feeds a strict recursive consumer and the
  intermediate aggregate is purely structural.

These are intentionally optional and conservative. They should:
- run before CPS conversion, where recursive shape is still explicit
- initially target classical-only code
- treat failure as "do nothing"
- feed simpler workers into the later shape-inference / bounded-recursion pass

They are best understood as Bucket 0 enablers: they may eliminate recursion
entirely or expose a simpler bounded traversal, but they do not replace static
shape inference or `CFor` lowering.

### 1. Static shape inference (`src/StaticShape.hs`)

Infer finite list lengths and aggregate shapes from top-level bindings:

```haskell
data Size
  = SizeKnown Int
  | SizeVar Variable
  | SizeUnknown

data Shape
  = ShapeScalar | ShapeQubit | ShapeBool
  | ShapeTuple [Shape]
  | ShapeList Size Shape
  | ShapeUnknown
```

Examples:
- `init_n n` produces `List Qubit` of length `n`
- `meas_all xs` preserves input list length
- `ghz_4` has concrete length `4`

Conservative: anything not proven bounded remains `SizeUnknown` and must be
rejected if it participates in recursion.

### 2. Bounded recursion lowering

Replace reject-only recursion handling with a pass that:
- Recognizes bounded recursive patterns
- Rewrites them into explicit loop IR, preserving the full loop structure
- Rejects everything that remains unsupported

`CFor` is generated for **all** bounded recursive functions, including pure
qubit-allocation loops like `init_n`. The IR faithfully represents the loop
even when the only operation is `PInit`. It is QubitHoist (not this pass) that
decides what the loop means at emission time — stripping `PInit` for OpenQASM,
or emitting dynamic allocation for QIR.

Recommended landing order:
1. Close the existing Class 3 correctness gap with early rejection.
2. Wire `StaticShape.hs` into the normal pipeline and extend it for the current
   missing multi-argument / composite cases.
3. Add the first bounded-recursion lowering path for counted and same-shape
   structural traversals, targeting `CFor`.
4. Add optional upstream specialization/fusion only where it simplifies real
   examples and preserves effect order without extra machinery.

Proposed CPS extension (`src/CPSExp.hs`):

```haskell
| CFor
    Variable      -- index variable
    Value         -- lower bound (inclusive) — usually VInt 0, but can be a VVar for nested loops
    Value         -- upper bound (inclusive) — static VInt (n-1), or dynamic VVar for inner loops
    CExp          -- body
    CExp          -- exit continuation
```

Using explicit lower/upper bounds (both `Value`) rather than a single trip count
allows the inner loop of a nested pair to start at `i+1` where `i` is the outer
index variable. The cost is negligible and avoids a constructor change later.

A `CFor` body is a `CExp`, so nesting is structural — an outer `CFor` body may
contain an inner `CFor`. Every traversal pass recurses into the body as normal.

List recursion should be lowered into counted iteration rather than a separate
list-specific loop node if possible.

### 3. Static list erasure (`src/StaticListErase.hs`)

After bounded list recursion is lowered:
- Erase `List a` values with known finite size into fixed records/tuples
- Remove `Nil`/`Cons` from the backend-facing IR
- Let existing record flattening handle the resulting aggregates

This is required because OpenQASM has no runtime algebraic data structures.

---

## Downstream pass requirements for `CFor`

Every pass between bounded recursion lowering and OpenQASM emission must be
updated to handle the new `CFor` constructor.

### ClosureConv, Defunc, RecordFlatten

Mechanical structural traversal cases — `CFor` is walked like other binding
forms. The body is a scope in which the index variable and loop-carried
variables are bound. No deep semantic change needed.

### QubitHoist

This is the most significant change. The transformation is **OpenQASM-specific**:
the IR faithfully preserves `PInit` inside `CFor` bodies (so a future QIR backend
can emit dynamic block allocation), but QubitHoist strips it for the OpenQASM
path, replacing it with a static array declaration.

Concretely, when `hoistExp` descends into a `CFor`:
1. Pre-scan the body to count `PInit` calls and compute the iteration count from
   the bounds.
2. Allocate `count × N` consecutive slots in a batch.
3. **Remove each `PInit` from the body** and inject `q → VQubitArr base (VVar i)`
   into the hoisting environment. The qubit variable is now available in the body
   via `VQubitArr` without an explicit allocation step.
4. If the `CFor` body is **empty after PInit removal** (all it did was allocate
   qubits — e.g. `init_n`), **drop the `CFor` node entirely**. The only OpenQASM
   output is the top-level `qubit[N] q;` declaration, which is emitted from the
   hoisted slot count as today. No `for` loop is emitted.
5. If the body still contains gate or measurement operations, keep the `CFor` with
   the PInit-stripped body.

For nested loops, the inner loop's `PInit` uses `VQubitArr base (VVar j)` where
`j` is the inner index. Each array is allocated a contiguous batch of slots.

`PInit` inside a `while` loop (Class 2) continues to allocate one slot (reused
each iteration because measurement collapses the qubit to `|0>`).

**QIR compatibility note:** a future QIR backend should use a different hoist
pass (or skip hoisting entirely) and instead emit the `CFor` with `PInit` as
dynamic per-iteration qubit allocation (`__quantum__rt__qubit_allocate()` or
equivalent). The IR representation is intentionally backend-neutral.

### `VQubitArr` — dynamic qubit indexing

A new `Value` constructor is needed to represent loop-indexed qubit references:

```haskell
| VQubitArr Int Value   -- qubit at static base slot + dynamic index value
```

`VQubitArr base (VVar i)` is the dynamic counterpart of `VQubit k`. It is
produced only by QubitHoist (never by earlier passes). All passes between
QubitHoist and emission handle it as a new `Value` case — mechanical in
ClosureConv and Defunc (already done by then), and in RecordFlatten and the
emitter. `extractQubitArg` in the emitter gains a `VQubitArr` branch.

The emitter renders `VQubitArr base idx` as `q[base + idx]` (or `q[idx]` when
`base = 0`).

### OpenQASM emitter

Add `StmtFor Variable Value Value [Stmt]` to the `Stmt` type (matching the
`CFor` lower/upper bound design) and handle `CFor` in `runExp`. The emitted
form is `for i in [lo:hi] { body }`. Qubit references inside the body use
`VQubitArr` and render as `q[base + i]`. The top-level `qubit[n] q;`
declaration is emitted from the hoisted slot count as today.

The emitter will never see a `CFor` whose body is empty after QubitHoist, since
QubitHoist drops such nodes. Any `CFor` reaching the emitter has at least one
gate or measurement statement in its body.

`CPrimOp PCpGate` currently requires a `ClassicalIntConst k` for the rotation
index. Inside a loop, `k` is a `ClassicalVar`. The emitter's `PCpGate` handler
must also accept a dynamic classical value and emit `cp(pi / pow(2, k))` rather
than a precomputed literal.

### StaticListErase dependency on QubitHoist

`StaticListErase` (pass 6 in the pipeline) erases `List a` of known size `n`
into a flat `n`-tuple before QubitHoist runs. This is required so that the
`CFor` body's qubit outputs have statically-addressable record fields, enabling
QubitHoist to assign concrete slot numbers. The passes must run in order:
StaticListErase → ClosureConv → Defunc → QubitHoist → RecordFlatten.

---

## Pipeline placement

```
1. Parse / scope / Lambda lowering
2. CPS conversion
3. Static shape inference        ← new
4. Bounded recursion lowering    ← new
5. Residual recursion rejection
6. Static list erasure           ← new
7. Interface flattening
8. Gate/def classification
9. Closure conversion
10. Defunctionalization
11. Qubit hoisting
12. Local record flattening
13. OpenQASM emission
```

---

## GHZ mapping

`examples/ghz.funq` needs all three planned pieces:
- `init_n`: counted recursion constructing a fixed-size list of qubits
- `cnot_layer`: structural recursion over a statically sized list with
  loop-carried qubit state
- `meas_all`: structural recursion over a statically sized list producing a
  fixed-size list of bits

Currently `ghz.funq` compiles via budget-unrolling (depth 1000). The three-pass
plan above is the right long-term replacement.

---

## Recommended implementation order

1. **Class 3 early rejection** in `OpenQASM.hs` — ✅ done
2. **Add `CFor` and `VQubitArr` to `CPSExp.hs`** — ✅ done; structural traversal cases added to all downstream passes
3. **StaticShape** (`StaticShape.hs`) — ✅ done; see [passes/static-shape.md](../passes/static-shape.md).
   Current limitations: multi-arg functions and composite bodies (ghz_n, qft_n) resolve
   to ShapeUnknown; countdown and single-arg list-map patterns are recognized.
4. **StaticListErase** (`StaticListErase.hs`) — erase `List a` of known size into fixed records; runs in `CompilePipeline.hs` before closure conversion
5. **Bounded recursion lowering** — detect countdown and structural list recursion patterns; emit `CFor` with static or dynamic bounds; reject dynamic trip counts on qubit-allocating functions
6. **QubitHoist** update — allocate N consecutive slots for `PInit` inside `CFor`; replace with `VQubitArr base (VVar i)`; handle nested loops correctly
7. **OpenQASM emitter** update — add `StmtFor`, handle `CFor` and `VQubitArr`, emit `for i in [lo:hi]` and `q[base + i]`; handle dynamic `PCpGate` rotation index
8. **Wire into `CompilePipeline.hs`** — insert StaticShape, StaticListErase, and bounded recursion lowering at the right stages
9. **Validate** — re-run all examples: `ghz.funq`, `qft_n.funq`, `rus.funq`, `countdown.funq`, `tele.funq`, `bell00.funq`

---

## Why the current approach is transitional

Budget-unrolling works for existing examples but is fragile because recursive
ADT semantics are currently split across three places:
- CPS lowering conventions (how `Nil`/`Cons` become tagged records)
- Interface flattening (how those tagged records flow across boundaries)
- Emitter-side `ValueRep` reconstruction (how the emitter reconstructs ADT
  shape during recursive evaluation)

The Phase 1–3 plan (shape inference → bounded recursion IR lowering → static
list erasure) moves this logic into the middle end where it belongs, and
simplifies the emitter to a pure OpenQASM renderer.
