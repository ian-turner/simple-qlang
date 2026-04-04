# Recursion Check and Tail-Loop Compilation

**Modules:** `src/RecElim.hs`, `src/BoundedRecursion.hs`, `src/CompilePipeline.hs`, `src/OpenQASM.hs`

See also: [../pipeline.md](../pipeline.md), [../future/bounded-recursion.md](../future/bounded-recursion.md)

---

## Overview

OpenQASM has no general runtime recursion. The compiler must either:
- Reject programs with unsupported recursion (compile-time error), or
- Compile bounded self-recursion to an OpenQASM `while` loop.

Three checks run in order before backend emission.

---

## 1. Local CFix recursion check (`RecElim.hs`)

For each `CFix` group, the pass intersects the set of bound function names with
the set of callee variables found anywhere in the function bodies. A non-empty
intersection means at least one function calls another (or itself) in the group.

- **Multi-function groups** with any recursion are **rejected** (mutual
  recursion is not supported).
- **Single-function groups** with self-recursion **pass through** — the
  self-recursive declaration proceeds to `BoundedRecursion.hs` for tail-loop
  classification, and ultimately to the emitter.

---

## 2. Top-level label cycle check (`CompilePipeline.hs`)

After per-declaration CPS lowering and the local recursion check,
`CompilePipeline.hs` performs a module-level analysis:

- Collects top-level `VLabel` callees from each declaration's CPS
- Builds a call graph over compiled declarations
- Marks every declaration in a cyclic strongly-connected component as a
  recursion error

This catches cases where top-level declarations form recursive label cycles
(`init_n -> init_n`, or mutual recursion across declarations). Such cycles
would otherwise reach interface flattening, classification, and the emitter.

Exceptions: declarations recognized by `BoundedRecursion.hs` as tail loops are
allowed through.

---

## 3. Tail-loop recognition and `while` compilation (`BoundedRecursion.hs` + `OpenQASM.hs`)

**Recognition (`BoundedRecursion.hs`):** identifies self-recursive declarations
where every recursive call passes the outer continuation unchanged — either
directly or via η-trivial chains (including Appel's curried-application
intermediates). These are "tail loops."

**Emission (`isTailLoop` in `OpenQASM.hs`):** a recognized tail loop emits as
an OpenQASM 3.0 `while` loop. The loop body is the function body; the
recursive call becomes the loop back-edge; the base case becomes the exit
condition.

```openqasm
// Example: rus_loop (repeat-until-success)
while (!m) {
  // loop body
}
```

**Fallback for non-tail-loop self-recursion:** guarded inline expansion with a
depth limit of 1000 calls. If the limit is reached, the compiler emits a
compile-time error. This handles bounded loops (e.g. `countdown.funq`) and
probabilistic repeat-until-success circuits (e.g. `rus.funq`) where the
recursion is provably finite but the bound may not be statically known.

---

## Supported recursion patterns

| Pattern | Handling |
|---|---|
| Single-function self-recursion (tail loop) | Compiled to `while` |
| Single-function self-recursion (non-tail-loop) | Inline expansion, depth 1000 |
| Multi-function `CFix` with any recursion | Compile-time error |
| Top-level label cycles | Compile-time error |
| Structural list recursion with dynamic bounds | Not yet supported — see [../future/bounded-recursion.md](../future/bounded-recursion.md) |

---

## Examples

| Example | Pattern | How compiled |
|---|---|---|
| `rus.funq` | `rus_loop` recurses with unchanged continuation | `while` loop |
| `countdown.funq` | `countdown_zero` recurses on `n-1` | `while` loop |
| `ghz.funq` | `init_n`, `cnot_layer`, `meas_all` recurse over lists | Budget-unrolling (depth 1000) |
| `qft_n.funq` | `init_n`, `meas_all`, `qft_core` recurse over lists | Budget-unrolling |
