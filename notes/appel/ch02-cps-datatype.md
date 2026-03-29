# Chapter 2 — Continuation-Passing Style

Pages 11–21. Status: **partial** (§2.1 read; §2.2–2.5 not yet read).

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

**`SELECT(i, v, w, E)`** — fetch the `i`th field of record `v`, bind to `w`,
continue with `E`. Fields are 0-indexed.

**`OFFSET(i, v, w, E)`** — adjust pointer `v` by `i` fields, bind to `w`.
Used for interior pointers into records.

**`APP(f, [a1,...,ak])`** — tail call `f` with arguments. No continuation
expression as a subterm — this is the "return" of CPS. Every call is a tail
call (p. 14).

**`FIX([(f1,params1,body1),...], E)`** — define mutually recursive functions,
then evaluate `E`. Functions are bound in `E` and in each other's bodies (p. 14).

**`SWITCH(val, [E0,E1,...])`** — multiway branch: if `val = i`, evaluate `Ei`.
Used for pattern matching on data constructor tags and conditional branches.

**`PRIMOP(op, vals, vars, conts)`** — primitive operation. Takes atomic value
arguments `vals`, binds result variables `vars`, continues with one or more
continuation expressions `conts`. The number of continuations depends on `op`:
- Arithmetic/gate ops: one continuation (pure sequencing)
- Comparison ops (`>`, `<`, etc.): two continuations — true branch, false branch (p. 13)
- **Measurement**: two continuations — `|0⟩` branch, `|1⟩` branch

### Atomicity invariant (p. 13)

All arguments to CPS operations are *atomic*: variables (`VAR`) or constants
(`INT`, `REAL`, etc.), never subexpressions. This means:
- Evaluation order is fully determined by the nesting of continuations
- Substitution (β-reduction) is always safe — no risk of duplicating effects
- Variables correspond closely to machine registers (noted p. 6)

### FunQ CPS datatype (Haskell sketch)

A direct translation of Appel's datatype to Haskell for FunQ:

```haskell
data Value
  = VVar  Var
  | VLabel Var          -- function labels (known call targets)
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

The crucial point: `PMeas` will always be used with exactly two `CExp`
continuations in the `[CExp]` list of `CPrimOp`. All gate `PrimOp`s use exactly
one. This invariant can be enforced by a smart constructor or a separate
`CMeas` constructor if desired.

### Example: `bell00` in CPS (see also `../cps-compilation-strategy.md`)

```
FIX([(bell00, [x, c],
       PRIMOP(init,  [],              [a],
       PRIMOP(init,  [],              [b],
       PRIMOP(hgate, [VAR a],         [a'],
       PRIMOP(cnot,  [VAR a', VAR b], [q1, q2],
       APP(VAR c, [TUPLE(VAR q1, VAR q2)])
       )))))],
E)
```

### Example: measurement with two continuations

```
PRIMOP(meas, [VAR q], [],
  [ (* |0> branch *) APP(VAR k, [BOOL False]),
    (* |1> branch *) APP(VAR k, [BOOL True]) ])
```

---

## 2.2 Functions that Escape (pp. 16–17)

*Not yet read.*

---

## 2.3 Scope Rules (pp. 17–18)

*Not yet read.*

---

## 2.4 Closure Conversion (pp. 18–21)

*Not yet read. Covered in depth in Chapter 10.*

---

## 2.5 Spilling (pp. 21–22)

*Not yet read. Covered in depth in Chapter 11.*
