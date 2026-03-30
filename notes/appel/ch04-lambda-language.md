# Chapter 4 — ML-Specific Optimizations

Pages 37–54. Status: **complete**.

---

## Overview

Chapter 4 describes transformations applied to ML programs *before* CPS
conversion. The pipeline produces a small intermediate language — *mini-ML* —
whose concrete data structure is called the **lambda language** (`lexp`,
Figure 4.1, p. 51). CPS conversion (Chapter 5) operates on `lexp`, not on the
raw ML AST.

Most of §4.1–4.4 is ML-specific (polymorphic boxing, runtime tags, ref cells,
GC). Sections 4.2, 4.5, and 4.7 are directly relevant to FunQ.

---

## 4.1 Data Representation (pp. 37–42) — low relevance

The main concern is how ML's polymorphic datatype constructors are represented
at runtime (tagged records, unboxed constants, transparent single-constructor
types, etc.). This matters because the compiler must choose representations
before types are erased during CPS conversion.

**Summary of constructor classifications (p. 42):**

| Class | Description |
|---|---|
| `Constant` | No payload; represented as a small integer (tag only) |
| `Tagged` | Two-word record: `(value, tag)` |
| `Transparent` | Only one value-carrying constructor; no tag needed |
| `TransB` / `TransU` | Transparent boxed / unboxed variants |

**FunQ relevance:** We don't target a GC heap, so the boxing/unboxing details
don't apply. What does apply: when generating `SWITCH` arms in CPS (for
constructor tests), constant constructors become integer cases (0, 1, ...).
For FunQ, `True` = 1, `False` = 0; `Nil` = 0, `Cons` = 1 (or similar).

---

## 4.2 Pattern Matching (pp. 43–44) — relevant

ML pattern matching compiles to a *decision tree* (after Maranget [19]).
Each internal node tests exactly one constructor tag; branches correspond to
possible constructors.

The result for mini-ML: every `case` expression tests exactly one level of
one datatype. Multi-level patterns like `(false, x::nil) => ...` become nested
`case` expressions. For example (p. 44), the ML match:

```
case a of
  (false, nil)   => nil
  (true, w)      => w
  (false, x::nil) => x::x::nil
  (false, y::z)  => z
```

becomes a tree of single-level tests: first test the first component (bool),
then test the second component (list constructor).

**Variable binding in case arms:** Variables bound in ML patterns do *not*
appear in mini-ML case rules. Instead, a `decon_c` primitive extracts the
carried value from a constructor. Example (p. 47–48):

```
-- ML:  | E x => e(x)
-- mini-ML:
  E _ => (fn x => e(x))(decon_E j)
```

where `decon_c(e) = case e of c x => x | _ => error`. The `decon_c`
operator is only called in a context guaranteed to have constructor `c`.

**FunQ relevance:** FunQ's `Case Exp [Alt]` must be lowered to flat,
single-level tests. Each `Alt (xs :. (pat, rhs))` becomes:
1. A `LSwitch` arm testing the constructor tag (using `LDecon` for the
   payload if the constructor carries a value)
2. Nested `LSwitch` for tuple sub-patterns

---

## 4.5 The mini-ML Sublanguage (pp. 46–48) — key section

Mini-ML is the untyped IR into which all Standard ML programs are translated
before CPS conversion. Its key constraints relative to ML:

**Single-argument lambdas only.** Multi-argument functions are uncurried using
tuple projections. The pattern `fn (x,y,z) => M` becomes:

```
fn xyz => (fn x => (fn y => (fn z => M)
                    (#3 xyz))
           (#2 xyz))
    (#1 xyz)
```

where `#i` selects the *i*th field (0-indexed in Appel's lambda language).

**No `let` expressions.** `let x = e1 in e2` is encoded as
`(fn x => e2)(e1)`.

**No variable binding in `case`.** Carried values are extracted by `decon_c`
projections after the constructor test (see §4.2 above).

**Simple case rules only.** Each rule is one of:
- `c => E` — constant constructor
- `c _ => E` — value-carrying constructor (value extracted via `decon_c`)
- `_ => E` — wildcard / default

Case expressions range over integer, real, string, and datatype values.

---

## 4.7 The Lambda Language (pp. 50–51) — key section

The lambda language is the concrete data structure (ML datatype) encoding
mini-ML. CPS conversion in Chapter 5 takes `lexp` as input (Figure 4.1, p. 51):

```
datatype lexp
  = VAR    of var
  | FN     of var * lexp              -- single-argument lambda
  | FIX    of var list * lexp list * lexp  -- mutually recursive defs
  | APP    of lexp * lexp             -- single-argument application
  | INT    of int
  | REAL   of string
  | STRING of string
  | SWITCH of lexp                    -- scrutinee
           * conrep list              -- constructor representations
           * (con * lexp) list        -- (constructor, branch) pairs
           * lexp option              -- default/wildcard branch
  | CON    of conrep * lexp           -- apply a constructor
  | DECON  of conrep * lexp           -- strip a constructor, get carried value
  | RECORD of lexp list               -- n-tuple construction
  | SELECT of int * lexp              -- field selection (0-indexed)
  | RAISE  of lexp                    -- raise exception
  | HANDLE of lexp * lexp             -- exception handler
  | PRIM   of primop                  -- primitive operation
```

Note: `FIX` in the lambda language takes three parallel lists
`(names, bodies, cont)` — slightly different from the CPS `FIX` which takes a
list of triples `(name, params, body)`.

`SWITCH` includes a `conrep list` (constructor representation metadata) to
guide code generation. For FunQ we can simplify this to just a tag integer.

The `PRIM` constructor here is a *value* — a first-class function wrapping a
primitive — distinct from CPS `PRIMOP` which is an *expression* taking
continuations. In lambda language, `PRIM p` applied to arguments via `APP`
is what CPS converts into `PRIMOP(p, ...)`.

---

## FunQ Lambda IR Design

Based on Chapter 4, the FunQ lowering pass should produce a `LExp` type
analogous to `lexp`, stripped of ML-specific constructs:

```haskell
data LExp
  = LVar    Variable
  | LLit    Lit                          -- Int, Float, Bool, String, Unit
  | LApp    LExp LExp                    -- single-argument application
  | LLam    Variable LExp                -- single-argument lambda
  | LFix    [(Variable, Variable, LExp)] LExp  -- mutually recursive functions
  | LTuple  [LExp]                       -- n-tuple construction
  | LSelect Int LExp                     -- select ith field (0-indexed)
  | LCon    String LExp                  -- apply constructor; LCon "Nil" LUnit for constant
  | LDecon  String LExp                  -- strip constructor, get carried value
  | LSwitch LExp [(ConAlt, LExp)] (Maybe LExp)  -- single-level constructor test
  | LPrim   PrimOp [LExp]               -- primitive operations (gates, arithmetic)

data Lit    = LInt Int | LFloat Float | LBool Bool | LString String | LUnit
data ConAlt = CATag Int | CACon String  -- match by integer tag or constructor name
```

Omitted vs. Appel: `RAISE`/`HANDLE` (FunQ has no exceptions), `REAL`/`STRING`
as separate constructors (folded into `LLit`), `conrep` metadata (not needed
until codegen).

---

## Lowering Pass: Syntax.Exp → LExp

The key transformations from FunQ's current resolved AST to `LExp`:

| Source (`Syntax.Exp`) | Target (`LExp`) |
|---|---|
| `Lam [v1,v2,...,vn] body` | Nested `LLam`: `LLam v1 (LLam v2 (...))` |
| `Let e (x :. body)` | `LApp (LLam x body) e` |
| `LetTuple e ([v1..vn] :. body)` | Bind to fresh `t`, then `LApp (LLam v1 ...) (LSelect 0 t)` etc. |
| `App f e` | `LApp f e` (already single-argument) |
| `Case scrut alts` | `LSwitch scrut [...]` — flatten patterns (see below) |
| `IfExp b t f` | `LSwitch b [(CACon "True", t), (CACon "False", f)] Nothing` |
| `BinOp op e1 e2` | `LPrim (opToPrimOp op) [e1, e2]` |
| `Base "hgate"` etc. | `LPrim PHGate []` etc. (partially applied; resolved at App) |
| `Con name` | `LCon name (LLit LUnit)` for constant; `LLam v (LCon name (LVar v))` for value-carrying |
| `Tuple [e1..en]` | `LTuple [e1..en]` |
| `Var v` / `Const s` / `Base s` | `LVar v` / reference to top-level name |
| Top-level `Def f (Lam ...)` | `LFix [(f, v, body)] cont` |

**Pattern flattening** — the most complex transformation. A `Case` with nested
patterns like `Alt (xs :. (PTuple [PFVar a, PFVar b], rhs))` becomes:
1. Test the constructor tag via `LSwitch`
2. For value-carrying constructors: `LDecon` to extract payload
3. For tuple sub-patterns: `LSelect 0 payload` bound to `a`, `LSelect 1 payload` bound to `b`

**Multi-argument base primitives** — FunQ's `init`, `hgate`, etc. are called
as `Base "hgate"` applied to arguments. After lowering, they become
`LPrim PHGate [arg]`. The lowering recognizes `App (Base p) e` and `App (App (Base p) e1) e2`
patterns and converts them to the appropriate `LPrim`.

---

## Sections Not Relevant to FunQ

- **§4.3 Equality** — polymorphic equality; not in FunQ
- **§4.4 Unboxed updates** — ML ref cells and `:=`; not in FunQ
- **§4.6 Exception declarations** — exceptions; not in FunQ
- **§4.8 Module system** — structures and functors; FunQ has no module system
