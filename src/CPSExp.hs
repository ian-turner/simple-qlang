module CPSExp where

import Utils (Variable)
import LambdaIR (PrimOp)


-- | Access paths into heap records (Appel §2.1)
data AccessPath
  = OFFp Int               -- offset by n words from a pointer
  | SELp Int AccessPath    -- select field then continue path
  deriving (Show, Eq)

-- | Atomic CPS values.  All arguments to CPS operations must be atomic.
--   Booleans are represented as integers (False = 0, True = 1).
data Value
  = VVar   Variable    -- local variable
  | VLabel String      -- top-level function label (resolved post-linking)
  | VInt   Int         -- integer or boolean constant (False=0, True=1)
  | VQubit Int         -- statically assigned backend qubit slot
  | VUnit              -- unit constant
  deriving (Show, Eq)

-- | CPS expression type (Appel Figure 2.1, adapted for FunQ).
--
-- Invariants:
--   - All arguments to every constructor are atomic Values.
--   - CApp has no continuation subexpression — every call is a tail call.
--   - CPrimOp PMeas always has exactly two continuation expressions.
--   - All other CPrimOp have exactly one continuation expression.
data CExp
  = CRecord  [(Value, AccessPath)] Variable CExp
    -- ^ Allocate a heap record, bind address to Variable, continue.
  | CSelect  Int Value Variable CExp
    -- ^ Fetch field i of record Value, bind to Variable, continue.
  | CApp     Value [Value]
    -- ^ Tail call: apply Value to argument list (no continuation subterm).
  | CFix     [(Variable, [Variable], CExp)] CExp
    -- ^ Mutually recursive function definitions, then continue.
    --   Each triple: (function name, parameter list, body).
  | CSwitch  Value [CExp]
    -- ^ Branch: if Value == i, evaluate the ith CExp in the list.
  | CPrimOp  PrimOp [Value] [Variable] [CExp]
    -- ^ Primitive operation.
    --   [Value]    — argument values
    --   [Variable] — result variables bound in the continuation(s)
    --   [CExp]     — continuation expression(s):
    --                  one for pure/gate ops, two for PMeas
  | COffset  Int Value Variable CExp
    -- ^ Interior pointer into a heap record.
    --   Bind (base + n words) to Variable, continue.
    --   Used to give each function in a shared closure its own field-0 code pointer.
  deriving (Show)
