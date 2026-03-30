# Chapter 2 — Continuation-Passing Style

Pages 11–22. Status: **complete**.

---

## 2.1 The CPS Datatype (pp. 11–16)

### Design choice: a typed ML datatype, not a Scheme convention

Earlier CPS compilers (cited on p. 11) represented CPS as Scheme programs
satisfying syntactic conventions. SML/NJ instead uses a concrete ML datatype
(`cexp`) that structurally enforces CPS well-formedness. The type system rules
out malformed expressions at the representation level.

This is the approach FunQ should follow: define a Haskell datatype for CPS
expressions that encodes the invariants (all arguments are atomic, all calls are
tail calls) by construction.

### The `cexp` datatype (Figure 2.1, p. 12)

```
datatype value
  = VAR of var | LABEL of var | INT of int | REAL of string | STRING of string

datatype accesspath
  = OFFp of int | SELp of int * accesspath

datatype cexp
  = RECORD  of (value * accesspath) list * var * cexp
  | SELECT  of int * value * var * cexp
  | OFFSET  of int * value * var * cexp
  | APP     of value * value list
  | FIX     of (var * var list * cexp) list * cexp
  | SWITCH  of value * cexp list
  | PRIMOP  of primop * value list * var list * cexp list
```

### Constructor meanings

**`RECORD(fields, w, E)`** — allocate a heap record, bind its address to `w`,
continue with `E`. Objects created with `RECORD` are immutable (p. 14).

Example: `RECORD([(VAR a, OFFp 0), (INT 2, OFFp 0), (VAR c, OFFp 0)], w, E)`
allocates a three-word record `(a, 2, c)`, binds its address to `w`.

**`SELECT(i, v, w, E)`** — fetch the `i`th field of record `v`, bind to `w`,
continue with `E`. Fields are 0-indexed.

**`OFFSET(i, v, w, E)`** — adjust pointer `v` by `i` fields, bind to `w`.
Used for interior pointers into records. The constant `j + i` must be
non-negative (where `j` is the field offset `v` already points to).

**`APP(f, [a1,...,ak])`** — tail call `f` with arguments. No continuation
expression as a subterm — this is the "return" of CPS. Every call is a tail
call (p. 14). There is no runtime stack: continuations *are* the return mechanism.

**`FIX([(f1,params1,body1),...], E)`** — define mutually recursive functions,
then evaluate `E`. Functions are bound in `E` and in each other's bodies (p. 14).
The effect is: define the `f_i`, then evaluate `E`, which typically calls one
of them or passes one as an argument.

**`SWITCH(val, [E0,E1,...])`** — multiway branch: if `val = i`, evaluate `Ei`.
Used for pattern matching on data constructor tags and conditional branches.
If the runtime value of `i` is negative or >= the list length, the expression
is erroneous.

**`PRIMOP(op, vals, vars, conts)`** — primitive operation. Takes atomic value
arguments `vals`, binds result variables `vars`, continues with one or more
continuation expressions `conts`. The number of continuations depends on `op`:
- Arithmetic/gate ops: one continuation (pure sequencing)
- Comparison ops (`>`, `<`, etc.): two continuations — true branch, false branch
  - Example: `PRIMOP(>, [VAR a, VAR b], [], [F, G])` — zero result vars, two continuations
- **Measurement**: two continuations — `|0⟩` branch, `|1⟩` branch

### Atomicity invariant (p. 13)

All arguments to CPS operations are *atomic*: variables (`VAR`) or constants
(`INT`, `REAL`, etc.), never subexpressions. This means:
- Evaluation order is fully determined by the nesting of continuations
- Substitution (β-reduction) is always safe — no risk of duplicating effects
- Variables correspond closely to machine registers (noted p. 6)

For expressions like `(a + 1) * (3 + c)`, CPS forces explicit naming of every
intermediate result:

```
u = a + 1,  v = 3 + c,  e = u * v
```

which in CPS becomes:
```
PRIMOP(+, [VAR a, INT 1], [u],
  [PRIMOP(+, [INT 3, VAR c], [v],
    [PRIMOP(*, [VAR u, VAR v], [e], [M])])])
```

This also means the compiler must commit to evaluation order during CPS
conversion (here, `a+1` before `3+c`). An optimizer can later reorder if safe.

### Worked example: `f(a+b) * f(c+d)` in CPS (p. 15)

Source: `let fun f(x) = 2*x+1 in f(a+b)*f(c+d) end`

CPS form (two continuation functions `k1` and `k2`):

```
FIX([(f, [x,k], PRIMOP(*, [INT 2, VAR x], [u],
                [PRIMOP(+, [VAR u, INT 1], [v],
                        [APP(VAR k, [VAR v])])]))],
  FIX([(k1, [i], FIX([(k2, [j],
                        PRIMOP(*, [VAR i, VAR j], [w],
                        [APP(VAR r, [VAR w])]))],
                      PRIMOP(+, [VAR c, VAR d], [m],
                      [APP(VAR f, [VAR m, VAR k2])])))],
    PRIMOP(+, [VAR a, VAR b], [n],
    [APP(VAR f, [VAR n, VAR k1])])))
```

`k1` and `k2` are *continuations* — they express "the rest of the computation"
after each call to `f` returns. The return value is passed as an argument to the
continuation rather than onto a stack.

### FunQ CPS datatype (Haskell sketch)

A direct translation of Appel's datatype to Haskell for FunQ:

```haskell
data Value
  = VVar  Var
  | VLabel Var          -- function labels (known call targets, post-closure)
  | VInt  Int
  | VBool Bool
  | VUnit

data PrimOp
  = -- Classical arithmetic
    PAdd | PSub | PMul | PDiv
  | PEq  | PLt  | PGt
  -- Quantum gates (single continuation)
  | PInit                -- init : Unit -> Qubit
  | PHGate               -- hgate : Qubit -> Qubit
  | PCNot                -- cnot  : (Qubit, Qubit) -> (Qubit, Qubit)
  -- Measurement (two continuations: |0>, |1>)
  | PMeas                -- meas  : Qubit -> Bool

data CExp
  = CRecord  [(Value, AccessPath)] Var CExp  -- heap allocation
  | CSelect  Int Value Var CExp              -- field selection
  | CApp     Value [Value]                   -- tail call (no continuation)
  | CFix     [(Var, [Var], CExp)] CExp       -- mutually recursive functions
  | CSwitch  Value [CExp]                    -- multiway branch
  | CPrimOp  PrimOp [Value] [Var] [CExp]    -- primitive op with continuations
```

`PMeas` is always used with exactly two `CExp` continuations in the `[CExp]`
list of `CPrimOp`. All gate `PrimOp`s use exactly one.

### Example: measurement with two continuations

```
PRIMOP(meas, [VAR q], [],
  [ (* |0> branch *) APP(VAR k, [BOOL False]),
    (* |1> branch *) APP(VAR k, [BOOL True]) ])
```

---

## 2.2 Functions that Escape (pp. 16–17)

A function *escapes* if it is exported or passed to code outside its defining
compilation unit — i.e., the compiler cannot see all call sites.

**Non-escaping functions** (used only locally): can be optimised freely —
unused arguments removed, calling convention changed, etc. — because the
compiler controls every call site.

**Escaping functions**: their representation must be stable. The compiler
cannot remove or reorder arguments because other compilation units may call
them with a fixed protocol.

### Escaping function conventions in ML-CPS (p. 17)

When ML is converted to CPS, the following invariants hold for escaping
functions (and the optimizer must preserve them):

- Every escaping *user function* has **one or two arguments**: the data
  argument (an n-tuple from ML's one-argument rule), plus optionally a
  continuation argument.
- Every escaping *continuation function* is a **one-argument** function.
- The exception handler (`sethdlr` primop) is always a one-argument escaping
  continuation.

These invariants allow the optimizer to reason about escaping functions even
without seeing their bodies.

### FunQ relevance

Quantum subroutines exposed as library functions (e.g., a `bell00` that can
be called from other modules) are escaping functions. Their argument
conventions must be fixed. Crucially:

- The linear type system already prevents a qubit argument from being dropped
  or duplicated, but it does not prevent a caller from passing the wrong number
  of arguments.
- The CPS representation must therefore respect fixed arities for escaping
  quantum subroutines.

---

## 2.3 Scope Rules (pp. 17–18)

CPS has simple, strict lexical scope. Every variable is bound in exactly one
place and cannot be used outside its syntactic scope.

### Binding rules by constructor

| Constructor | Bound variable | Scope |
|---|---|---|
| `PRIMOP(p, vl, [w], [e1, e2, ...])` | `w` | `e1, e2, ...` |
| `RECORD(vl, w, e)` | `w` | `e` |
| `SELECT(i, v, w, e)` | `w` | `e` |
| `OFFSET(i, v, w, e)` | `w` | `e` |
| `FIX([(f,[w1,...],b),...], e)` | each `w_i` | its own body `b` |
| `FIX([(f,[w1,...],b),...], e)` | each `f` | all bodies `B_j` and `e` |
| `APP`, `SWITCH`, `PRIMOP` with empty vars | — | (no binding) |

**Single-assignment**: once a variable is bound, it holds the same value
throughout its scope and cannot be reassigned. (A body executed on multiple
calls binds a fresh instance each time, but within one execution the value is
fixed.)

### FunQ relevance

The single-binding property is directly analogous to linear variable usage:
each qubit variable `q` is bound once (at `init` or as a function parameter)
and consumed once (at `meas` or passed to another function). CPS scope rules
make qubit liveness syntactically explicit — the scope of `q` is exactly the
region where it is live.

---

## 2.4 Closure Conversion (pp. 18–21)

### The problem: free variables

CPS functions can have *free variables* — variables defined outside the
innermost-enclosing function. A von Neumann machine represents a function as a
code address only; it cannot represent free-variable bindings. Example: `k2`
on p. 15 refers to `i` (defined in `k1`'s body) and `j` (its own argument).
`j` is bound, `i` is free.

### The solution: closures

A *closure* is a pair `(code-pointer, free-variable-record)`. When a function
is passed or stored, it is the closure that travels, not the raw code pointer.
To call a closure `f`:
1. Extract `f'` = first field of `f` (the code pointer).
2. `APP(f', [f, other_args...])` — pass the closure itself as the first arg,
   so `f'` can find its free variables in the record.

### Post-closure CPS invariant: the free-variable rule

After closure conversion, the only free variables in a function body `B_i` are:
- Its own formal parameters `v_ik`, and
- The names `f_j` of co-defined functions in the same `FIX` — but these are
  referenced as **`LABEL`** (constants), not `VAR`.

**`LABEL` vs `VAR`**: function names after closure conversion become `LABEL`
values (code addresses, essentially constants). From the register allocator's
perspective, a `VAR` occupies a register; a `LABEL` does not.

### Flat top-level form

Since no function has nontrivial free variables, nesting is unnecessary. The
entire compilation unit collapses into a single top-level `FIX`:

```
FIX([ (f0, [v01, v02, ..., v0m0], E),
      (f1, [v11, v12, ..., v1m1], B1),
      (f2, [v21, v22, ..., v2m2], B2),
      ...
      (fn, [vn1, vn2, ..., vnmn], Bn) ],
    APP(VAR f0, [VAR v01, VAR v02, ..., VAR v0m0]))
```

None of the `B_i` or `E` contain a nested `FIX`. The free variables of `E`
become formal parameters of the wrapper `f0`.

### Free variable computation (p. 21)

The formal `fv()` function (used in the closure-conversion algorithm, Ch 10):

```
fv(APP(v, la))                         = fvl(v::la)
fv(SWITCH(v, [C1, C2, ...]))           = fvl[v] ∪ ⋃_i fv(C_i)
fv(RECORD([(v1,p1),(v2,p2),...], w, E)) = fvl[v1,v2,...] ∪ fv(E) − {w}
fv(SELECT(i, v, w, E))                 = fvl[v] ∪ fv(E) − {w}
fv(OFFSET(i, v, w, E))                 = fvl[v] ∪ fv(E) − {w}
fv(PRIMOP(p, la, [w1,...], [C1,...]))  = fvl(la) ∪ ⋃_i fv(C_i) − ⋃_j {w_j}
fv(FIX([(f1,[w11,...],B1),...], E))    = (fv(E) ∪ ⋃_i(fv(B_i) − ⋃_j{w_ij})) − ⋃_i{f_i}
```

### FunQ deviation: qubits cannot be captured in closures

This is the most critical difference between classical and quantum closure
conversion. In classical CPS, free variables are copied into the closure
record. But **qubits are linear** — they cannot be copied (no-cloning theorem).

Consequence:
- A qubit must never appear as a free variable of a function.
- The FunQ linear type system enforces this statically: a qubit variable in
  scope must be consumed (passed or measured) in the same function body where
  it is bound; it cannot be "captured" by a nested lambda.
- In practice: closure conversion for FunQ can follow Appel's algorithm
  exactly, because the type checker guarantees qubit-free closures before
  this phase runs.

---

## 2.5 Spilling (pp. 21–22)

### The problem: unbounded variables vs. finite registers

CPS variables correspond closely to machine registers. But CPS expressions can
have arbitrarily many simultaneously-live variables, while real machines have
at most `k` registers.

**Liveness in CPS = free variables**: a variable is live at a point if it
appears free in the continuation expression from that point onward. The
free-variable function `fv()` defined above computes exactly this. This is
equivalent to the standard dataflow liveness algorithm.

### The finite-register rule

> **For compilation to a machine with `k` registers, no subexpression of the
> CPS may have more than `k` free variables.**

A stricter variant also applies before the spill phase:

> **No function of the CPS may have more than `k` formal parameters.**

The *spill phase* (Chapter 11) rewrites the CPS to satisfy these rules by
storing excess live variables to heap records and reloading them when needed.

### FunQ deviation: qubit spilling is not classical spilling

For classical variables, spilling means storing to memory (heap record) and
reloading with `SELECT`. For qubits, this is impossible — qubits cannot be
stored in classical memory.

Options for qubit spilling in FunQ:

1. **Static rejection**: treat exceeding the qubit register count as a
   compile-time error. The compiler bounds the required qubit count and
   rejects programs that exceed hardware capacity.
2. **Teleportation-based spilling**: theoretically possible but expensive
   (requires ancilla qubits and classical communication). Not practical for
   most compilation targets.
3. **Ancilla recycling**: after measurement, a qubit register is freed. The
   spill phase should aggressively reclaim measured qubits rather than keeping
   them live.

In practice for FunQ, option 1 is the most pragmatic starting point: compute
the maximum qubit width (maximum number of simultaneously live qubits) and
report an error if it exceeds the target hardware's qubit count. This is
analogous to Appel's `k`-register constraint, but with no spill-to-memory
fallback for qubits.

Classical free variables (booleans, integers) in FunQ CPS can be spilled
normally using Appel's algorithm.
