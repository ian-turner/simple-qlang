# Closure Conversion

**Module:** `src/ClosureConv.hs`
**Input:** interface-flattened `CExp`
**Output:** flat closed `CExp` with a single top-level `CFix`

See also: [../pipeline.md](../pipeline.md), [../quantum-semantics.md](../quantum-semantics.md), [../appel/ch10-closure-conversion.md](../appel/ch10-closure-conversion.md)

---

## Overview

`ClosureConv.hs` eliminates lexical free variables from every `CFix`-bound
function. Nested `CFix` groups are lifted into one top-level `CFix`, and any
function value that can flow at runtime becomes an explicit closure record.

After this pass:
- No lifted function body refers directly to outer lexical bindings
- Indirect calls through variables use an explicit closure-passing convention
- The declaration is a flat CPS program with at most one top-level `CFix`

---

## Current strategy

The implementation is intentionally conservative: every `CFix`-bound function
is treated as escaping and therefore gets a closure.

For a `CFix` group with `n` sibling functions and `m` shared free variables,
the pass builds one shared flat record:

```text
RECORD(code_0, code_1, ..., code_{n-1}, fv_0, ..., fv_{m-1})
```

Then:
- Each original function name is rebound to an `OFFSET` into that shared record
- Each lifted function gains a new first parameter `clo`
- `buildPreamble` reconstructs sibling closures with `COffset (k - i)`
- Shared free variables are loaded from the closure with `CSelect (n + j - i)`
- Synthetic code labels are generated as `_fun0`, `_fun1`, ...

This matches the shared-closure layout described in Appel Ch 10, but FunQ
currently skips the known-function optimization and goes straight to closures.

---

## Free-variable analysis

Only `VVar` counts as a free variable. `VLabel` is treated as a static label,
not a runtime environment dependency.

The analysis removes binders introduced by:
- `CRecord`
- `CSelect`
- `COffset`
- `CFix`
- `CPrimOp`
- `CFor`

This keeps the closure environment limited to runtime values that truly cross a
lexical boundary.

---

## Call rewriting

An indirect call through a variable is rewritten to fetch the code pointer from
field 0 and pass the closure itself as the new first argument:

```text
CApp (VVar v) args
  =>
CSelect 0 (VVar v) cp
  (CApp (VVar cp) (VVar v : args))
```

Direct `VLabel` calls are left unchanged.

`Defunc.hs` depends on this exact pattern when it rewrites indirect closure
calls into `CSwitch`-based dispatch.

---

## FunQ-specific constraints

- Well-typed FunQ programs must not capture qubits in closures. That invariant
  comes from [../quantum-semantics.md](../quantum-semantics.md), and it is what
  makes Appel-style closure conversion safe here.
- Interface record flattening runs before closure conversion so callable
  parameter structure is still visible before closure records obscure it.
- The current OpenQASM emitter does not consume the closure-converted IR
  directly; it emits from the earlier interface-flattened form for readability
  and simpler backend logic.

---

## Current limitations

- No known-function versus escaping-function split; all nested functions are
  closure allocated
- No callee-save continuation optimization
- No stack allocation of downward-only closures
- No attempt to minimize shared closure payloads per call site or per function

These are acceptable for now because the pass exists to establish a correct,
closed CPS form for downstream passes, not to optimize runtime closure layout.
