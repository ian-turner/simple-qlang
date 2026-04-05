# Static Shape Analysis (`StaticShape.hs`)

Infers list sizes and aggregate shapes for all top-level bindings.
Runs after CPS conversion and before bounded recursion lowering (pipeline steps 3 → 4).

---

## Purpose

Bounded recursion lowering needs to know, at each call site, whether the
argument to a list-recursive function has a statically determinable size.
`StaticShape` computes that information in a whole-module fixed-point pass.

---

## Data types

```haskell
data Size
  = SizeKnown Int        -- concrete size known at compile time
  | SizeVar   Variable   -- size equals the value of this variable:
                         --   Int param → its runtime value
                         --   List param → its length
  | SizeUnknown

data Shape
  = ShapeScalar          -- Int / Bool / Unit (undifferentiated)
  | ShapeQubit           -- quantum bit
  | ShapeBool            -- classical measurement result
  | ShapeTuple [Shape]   -- fixed-size tuple
  | ShapeList Size Shape -- homogeneous list
  | ShapeFun Variable Shape  -- function: param var + return shape (may contain SizeVar)
  | ShapeUnknown
```

`ShapeFun v ret` records which variable (`v`) is the lambda parameter so that
`applyShape` can substitute `SizeVar v` occurrences in `ret` when called with
a concrete argument.

---

## Algorithm

Fixed-point iteration over all top-level declarations, operating on `LExp`
(Lambda IR).  Each iteration refines the shape of every binding using shapes
computed for other bindings in the previous iteration.  12 iterations is the
hard limit (enough for any current program).

For each declaration:
1. If the LExp is an `LLam v body`, try the two specific pattern recognizers
   before falling back to general expression shape inference.
2. Otherwise (a constant value), run general expression shape inference.

### Pattern recognizers

**Countdown list constructor** (`recognizeCountdown`):

```
f(n) = if n == 0 then Nil else Cons elem (f (n-1))
```

Recognized from the lowered `LSwitch (PEq [n, 0]) [True→Nil, False→Cons]`
structure.  Infers `ShapeList (SizeVar n) elem_shape`.

**Structural list map / same-size recursion** (`recognizeListMap`):

```
f(xs) = case xs of Nil → Nil; Cons h t → Cons (g h) (f t)
```

Recognized from the lowered `LApp (LLam t (LSwitch (LVar t) [Nil, Cons] _)) (LVar xs)`
structure.  The Cons arm is walked through let-binding chains (tracking bound
variables in a `BindMap`) to find `Cons head (self-call tail)`.  This handles
the common pattern where the recursive call is bound to a variable before the
final Cons:

```haskell
let rest = qft_core qs'' in Cons q'' rest
```

Infers `ShapeList (SizeVar xs) elem_shape`.

### Shape application

`applyShape (ShapeFun v ret) argShape argInt` substitutes all `SizeVar v`
occurrences in `ret`:
- If `argInt = Just n`: `SizeVar v → SizeKnown n`
- If `argShape = ShapeList s _`: `SizeVar v → s`
- Otherwise: `SizeVar v → SizeUnknown`

This enables call-site instantiation:
```
init_n : ShapeFun n (ShapeList (SizeVar n) ShapeQubit)
init_n 4 → ShapeList (SizeKnown 4) ShapeQubit
```

---

## What the pass can infer

| Binding | Inferred shape |
|---|---|
| `init_n n` | `ShapeFun n (ShapeList (SizeVar n) ShapeQubit)` |
| `meas_all xs` | `ShapeFun xs (ShapeList (SizeVar xs) ShapeBool)` |
| `qft_core qs` | `ShapeFun qs (ShapeList (SizeVar qs) ShapeUnknown)` |
| `init_n 4` (call site) | `ShapeList (SizeKnown 4) ShapeQubit` |

## Current limitations

- **Multi-argument functions** (`cnot_layer`, `apply_rotations`): neither
  pattern recognizer matches; shape is `ShapeFun v ShapeUnknown`.
- **Composite function bodies** (`ghz_n`, `qft_n`): general expression inference
  runs but cannot currently propagate through multi-step let chains that combine
  list construction with size arithmetic.  `ghz_4` and `qft_n 4` resolve to
  `ShapeUnknown` in the current pass.

These are acceptable for step 3.  Bounded recursion lowering (step 4) will
use concrete shapes where available and report errors for `SizeUnknown` cases
that involve qubit allocation.

---

## API

```haskell
analyzeModuleShapes  :: [(String, LExp)] -> ModuleShapes
lookupTopLevelShape  :: ModuleShapes -> String -> Shape
applyShape           :: Shape -> Shape -> Maybe Int -> Shape
listSize             :: Shape -> Maybe Size
```

`analyzeModuleShapes` takes `(name, lambdaIR)` pairs from the pipeline's
`compiledLambdaIR` fields.  Pipeline wiring is deferred to step 8.
