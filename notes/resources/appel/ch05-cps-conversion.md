# Chapter 5 — Conversion into CPS

Pages 55–66. Status: **complete**.

---

## Overview

Chapter 5 gives the recursive algorithm that converts lambda-language
expressions into CPS. The algorithm is a function F that takes two arguments:

- **E** — a lambda-language expression (`lexp` / `LExp` in FunQ)
- **c** — a *metalanguage* continuation function of type `Value → CExp`

The result is a CPS expression (`cexp` / `CExp`). The return value is produced
by `c`: wherever the original expression would "return" a value, F calls `c` on
that value, and `c` produces whatever CPS code should follow.

**Critical point:** `c` is a Haskell function in our implementation — it is
*not* a CPS variable or continuation expression. It exists at compile time only,
as a way of threading "what to do with the result" through the recursion. This
is why the algorithm can avoid code duplication: `c` can be wrapped in a FIX
(giving it a name) the first time we need to call it twice.

---

## 5.1 Variables and Constants (p. 55)

A variable is just handed off to the continuation:

```
F(L.VAR v,    c) = c(VAR v)
F(L.INT i,    c) = c(INT i)
F(L.REAL r,   c) = c(REAL r)
F(L.STRING s, c) = c(STRING s)    -- simplified; Appel also handles char encoding
```

### FunQ relevance

`LVar v → c(VVar v)`, `LLit (LInt i) → c(VInt i)`, etc. Straight translation.
FunQ has `LUnit` (the unit literal) — map to `c(VUnit)`.

---

## 5.2 Records and Selection (p. 56)

An empty record becomes the integer 0:

```
F(L.RECORD nil, c) = c(INT 0)
```

A non-empty record evaluates each field expression in turn (via the auxiliary
F_l), then allocates a RECORD once all field values are available:

```
F(L.RECORD Ā, c) = F_l(Ā, λā.RECORD(map(λv.(v, OFFp 0)) ā, x, c(VAR x)))
```

The auxiliary **F_l** converts a list of lambda-language expressions,
accumulating their CPS values into a list `w̄` (reversed during accumulation,
reversed back at the end):

```
F_l(Ā, c) =
  let g(E::R̄, w̄) = F(E, λv.g(R̄, v::w̄))
      g(nil,  w̄) = c(reverse w̄)
  in g(Ā, nil)
```

Selection evaluates the record expression, then applies SELECT:

```
F(L.SELECT(i, E), c) = F(E, λv.SELECT(i, v, w, c(VAR w)))
```

`w` is a freshly invented CPS variable that did not appear before.

### FunQ relevance

`LTuple` and `LSelect` map directly to these rules. `LTuple [e1,...,en]` uses
the non-empty RECORD rule (via F_l). `LSelect i e` uses the SELECT rule.

Note: in FunQ `OFFp 0` can be omitted from the record field representation
since we do not use interior-pointer access paths; each field is simply `(value,
OFFp 0)`.

---

## 5.3 Primitive Arithmetic Operators (p. 57–59)

Appel's rules for primops pattern-match on `L.APP(L.PRIM i, E)` because in the
lambda language `PRIM` is a first-class value applied via `APP`. FunQ's
`LPrim op args` already has arguments collected, so the FunQ conversion will
pattern-match directly on `LPrim`. The four categories and how they map:

### Category 1 — one result, one continuation (arithmetic, gates)

Standard arithmetic and single-qubit gates. One input expression, one result
variable, one continuation:

```
F(L.APP(L.PRIM i, E), c) = F(E, λv.PRIMOP(i, [v], [w], [c(VAR w)]))
```

For a multi-argument primop applied to a record `Ā`:

```
F(L.APP(L.PRIM i, L.RECORD Ā), c) = F_l(Ā, λā.PRIMOP(i, ā, [w], [c(VAR w)]))
```

**FunQ mapping:** `LPrim PAdd [e1, e2]`, `LPrim PHGate [e]`, `LPrim PInit []`,
`LPrim PCNot [e1, e2]` — all category 1. Each of these uses F_l to evaluate
the argument list, then emits a single-continuation PRIMOP.

### Category 2 — no result, one continuation (side-effect only)

Operators like array update and ref assignment execute for their side effect
and "return" a placeholder:

```
F(L.APP(L.PRIM i, E), c) = F(E, λv.PRIMOP(i, [v], [], [c(INT 0)]))
```

The `INT 0` placeholder is typically eliminated by the optimizer.

**FunQ mapping:** No current FunQ primops are category 2; this category is
included for completeness.

### Category 3 — branching primops (two continuations)

Comparison operators and, crucially for FunQ, **measurement**. These take an
argument and branch into two continuations. The naive approach would be:

```
-- WRONG: calls c twice, duplicating code
PRIMOP(i, [v], [], [c(INT 1), c(INT 0)])
```

The correct approach wraps `c` in a FIX to give it a name, then calls that
name from both branches:

```
F(L.APP(L.PRIM i, E), c) =
  F(E, λv.FIX([(k, [x], c(VAR x))],
       PRIMOP(i, [v], [], [APP(VAR k, [INT 1]),
                           APP(VAR k, [INT 0])])))
```

The FIX names the continuation `k` so that its body (`c(VAR x)`) is generated
only once. Both branches call `k` with 1 or 0 respectively.

This is the textbook Appel pattern for branching primops. FunQ originally
followed this shape for measurement, but the current implementation does not.

**Current FunQ mapping:** `LPrim PMeas [e]` lowers as an ordinary single-result
primop so the measured bit can flow forward without forcing immediate backend
branch duplication. The measurement rule is:

```
F(LPrim PMeas [e], c) =
  F(e, λv.
       CPrimOp PMeas [v] [w] [c(VVar w)])
```

Branching on the measured result is represented later through ordinary `SWITCH`
nodes over that classical value.

### Category 4 — callcc and throw

Special control operators for capturing the current continuation. Not relevant
to FunQ (see §5.9 below).

---

## 5.4 Function Calls (p. 59–60)

### Lambda abstraction

A function `fn v => E` in the lambda language becomes a CPS function with an
extra continuation argument `k`. When the body finishes, it calls `k` with
the result:

```
F(L.FN(v, E), c) =
  FIX([(f, [v, k], F(E, λz.APP(VAR k, [z])))],
      c(VAR f))
```

`f` is a fresh name for the function. The body is `F(E, λz.APP(VAR k, [z]))`:
whatever value E produces is passed to `k`. After defining `f`, we apply `c`
to `VAR f` — handing the function value to the surrounding context.

### Application

To call a function, a *return continuation* `r` is created that will receive
the result. Both the function expression `F` and the argument `E` are
evaluated (function first), then the CPS APP is made with both the argument and
the return continuation `r`:

```
F(L.APP(F, E), c) =
  FIX([(r, [x], c(VAR x))],
      F(F, λf.F(E, λe.APP(f, [e, VAR r]))))
```

`r` is the return continuation: when the call returns, `r` is invoked with the
result `x`, and control flows to `c(VAR x)`. Again, wrapping in FIX ensures
the continuation code is generated once.

### FunQ relevance

`LApp f e` maps directly to the application rule. `LLam v body` maps to the
lambda rule. The continuation parameter `k` is always the last argument.

---

## 5.5 Mutually Recursive Functions (p. 60)

Both lambda language and CPS have FIX, so the conversion is straightforward.
Each function in the mutually recursive group gains a continuation parameter:

```
F(L.FIX(h̄, b̄, E), c) = FIX(g(h̄, b̄), F(E, c))
```

where `g` converts each function body:

```
g(h1::h̄, L.FN(v, B)::b̄) = (h1, [v, w], F(B, λz.APP(VAR w, [z]))) :: g(h̄, b̄)
g(nil,    nil)            = nil
```

Each function `h_i` gets a fresh continuation parameter `w`; its body
converts `B` and calls `w` with the result.

### FunQ relevance

`LFix [(f1, v1, body1), ..., (fn, vn, bodyn)] E` maps directly. The triple
`(fi, vi, bodyi)` in FunQ's LFix corresponds to one entry of `(h̄, b̄)`.

---

## 5.6 Data Constructors (p. 60–61)

The lambda language represents constructors using `conrep` tags (CONSTANT,
TAGGED, TRANSPARENT, etc.). FunQ uses simpler `LCon`/`LDecon`. The relevant
Appel rules and their FunQ equivalents:

### Constructors

| Appel conrep | Appel rule | FunQ equivalent |
|---|---|---|
| `CONSTANT i` | `F(L.CON(CONSTANT i, E), c) = F(L.INT i, c)` | `LCon "Nil" _ → c(VInt tag)` |
| `TAGGED i` | `F(L.CON(TAGGED i, E), c) = F(L.RECORD[E, L.INT i], c)` | `LCon "Cons" e → allocate record (value, tag)` |
| `TRANSPARENT` | `F(L.CON(TRANSPARENT, E), c) = F(E, c)` | single-constructor type; identity |

FunQ uses string constructor names rather than `conrep` integers. Before CPS
conversion, each constructor name should be mapped to a numeric tag. For FunQ's
current built-in types: `False=0, True=1`; `Nil=0, Cons=1` (or similar).

### Deconstruction

| Appel conrep | Rule |
|---|---|
| `TAGGED(i)` | `F(L.DECON(TAGGED(i), E), c) = F(L.SELECT(0, E), c)` — fetch first field |
| `TRANSPARENT` | `F(L.DECON(TRANSPARENT, E), c) = F(E, c)` — identity |

**FunQ simplification:** FunQ's `LDecon name e` strips a single-field tagged
constructor. In the simple case (all FunQ value-carrying constructors have one
field or a tuple field), `LDecon name e` maps to `F(e, λv.SELECT(0, v, w, c(VAR w)))`.

---

## 5.7 Case Statements (p. 61–62)

The match compiler (Chapter 4 / FunQ's lowering pass) has already translated
complex pattern matches into flat `L.SWITCH` expressions with simple arms.
Converting a SWITCH to CPS is therefore straightforward:

```
F(L.SWITCH(E, arms, default), c) =
  F(E, λv.SWITCH(v, [F(arm_i, c) for each arm_i]))
```

The scrutinee is evaluated first, then the CPS SWITCH dispatches on the result
value. Each arm's expression is converted with the same continuation `c`.

Appel discusses three search strategies (linear, binary, jump table) for
matching an integer tag against multiple constructor cases. For FunQ:
- `Bool` switches (two cases): linear search is fine
- `Int` switches: jump table or binary search, but these arise rarely in practice

**FunQ simplification:** FunQ's LSwitch arms are tagged by `CACon name` or
`CATag Int`. Before CPS conversion, string constructor names should be resolved
to integer tags so the CPS SWITCH contains integer literals.

---

## 5.8 Exception Handling (p. 63–64)

Uses `gethdlr`/`sethdlr` primops to maintain a current exception handler in a
store location. Not relevant to FunQ, which has no exceptions.

---

## 5.9 Call with Current Continuation (p. 64–66)

`callcc` and `throw` for first-class continuation capture. Not relevant to FunQ.

---

## The Conversion Function — FunQ Summary

The Haskell type of the conversion function:

```haskell
-- c :: Value -> CExp  (metalanguage continuation)
cpsConvert :: LExp -> (Value -> CExp) -> CExp
```

Key rules written in Haskell pseudocode:

```haskell
cpsConvert (LVar v)          c = c (VVar v)
cpsConvert (LLit l)          c = c (litToValue l)

cpsConvert (LLam v body)     c =
  let f = fresh
      k = fresh
  in CFix [(f, [v, k], cpsConvert body (\z -> CApp (VVar k) [z]))]
          (c (VVar f))

cpsConvert (LApp func arg)   c =
  let r = fresh
      x = fresh
  in CFix [(r, [x], c (VVar x))]
          (cpsConvert func $ \f ->
           cpsConvert arg  $ \e ->
           CApp f [e, VVar r])

cpsConvert (LFix defs body)  c =
  let cpsDef (f, v, b) =
        let w = fresh
        in (f, [v, w], cpsConvert b (\z -> CApp (VVar w) [z]))
  in CFix (map cpsDef defs) (cpsConvert body c)

cpsConvert (LTuple es)       c = cpsConvertList es $ \vs ->
  let x = fresh
  in CRecord (zip vs (repeat (OFFp 0))) x (c (VVar x))

cpsConvert (LSelect i e)     c = cpsConvert e $ \v ->
  let w = fresh
  in CSelect i v w (c (VVar w))

-- Category 1 primop: one result, one continuation
cpsConvert (LPrim op args)   c   -- op ∉ {PMeas}
  | isSingleResult op = cpsConvertList (map lexpFromArg args) $ \vs ->
      let w = fresh
      in CPrimOp op vs [w] [c (VVar w)]

-- Measurement in current FunQ: one result, one continuation
cpsConvert (LPrim PMeas [e]) c = cpsConvert e $ \v ->
  let w = fresh
  in CPrimOp PMeas [v] [w] [c (VVar w)]

cpsConvert (LSwitch scrut arms def) c = cpsConvert scrut $ \v ->
  CSwitch v [cpsConvert body c | (_, body) <- arms]
  -- default arm handled separately if present

cpsConvert (LCon name e)     c = ...  -- see §5.6 above
cpsConvert (LDecon name e)   c = ...  -- see §5.6 above
```

The auxiliary `cpsConvertList` is F_l:

```haskell
cpsConvertList :: [LExp] -> ([Value] -> CExp) -> CExp
cpsConvertList []     c = c []
cpsConvertList (e:es) c = cpsConvert e $ \v ->
                          cpsConvertList es $ \vs ->
                          c (v:vs)
```

---

## Worked Example: `meas`

Source (after lowering): `LPrim PMeas [LVar q]`

Applying the current FunQ rule with continuation `c`:

```
CPrimOp PMeas [VVar q] [w]
  [ c(VVar w) ]
```

The measured classical bit flows into `w` and then into `c`. If the source
program later branches on that result, CPS lowering represents that explicitly
with `CSwitch` over `w`.

---

## Sections Not Relevant to FunQ

- **§5.8 Exception handling** — FunQ has no exceptions; `RAISE`/`HANDLE` do not appear in `LExp`
- **§5.9 Call with current continuation** — FunQ has no `callcc` or `throw`
- **ML string single-character encoding** (§5.1) — FunQ uses `LLit (LString s)` uniformly
- **ML `conrep` metadata** (§5.6) — FunQ resolves constructor tags to integers during lowering
