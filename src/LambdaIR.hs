module LambdaIR where

import Utils (Variable)


-- | Primitive operations available in FunQ
data PrimOp
  -- Quantum operations
  = PInit     -- init  : Unit -> Qubit
  | PMeas     -- meas  : Qubit -> Bool
  | PHGate    -- hgate : Qubit -> Qubit
  | PXGate    -- xgate : Qubit -> Qubit
  | PZGate    -- zgate : Qubit -> Qubit
  | PCNot     -- cnot   : Qubit -> Qubit -> (Qubit, Qubit)
  | PSGate    -- sgate  : Qubit -> Qubit
  | PTGate    -- tgate  : Qubit -> Qubit
  | PCSGate   -- csgate : Qubit -> Qubit -> (Qubit, Qubit)
  | PCTGate   -- ctgate : Qubit -> Qubit -> (Qubit, Qubit)
  -- Classical arithmetic
  | PAdd | PSub | PMul | PDiv
  -- Classical comparison
  | PEq | PLt | PGt | PLe | PGe
  -- Classical logic
  | PAnd | POr | PNot
  deriving (Show, Eq)

-- | Literal values
data Lit
  = LInt    Int
  | LFloat  Float
  | LBool   Bool
  | LString String
  | LUnit
  deriving (Show, Eq)

-- | Constructor tags for LSwitch arms
data ConAlt
  = CACon String  -- named constructor (e.g. "Nil", "Cons", "True", "False")
  | CAUnit        -- unit pattern
  deriving (Show, Eq)

-- | Lambda IR: the intermediate form between the resolved AST and CPS.
--
-- Constraints (analogous to Appel's mini-ML / lambda language, Ch 4):
--   - All lambdas are single-argument: multi-arg lambdas become nested LLam
--   - No let-expressions: let x = e in body ≡ LApp (LLam x body) e
--   - No variable binding in case arms: pattern variables extracted via LDecon / LSelect
--   - Constructor tests are single-level: nested patterns become nested LSwitch
data LExp
  = LVar    Variable                               -- variable reference
  | LLit    Lit                                    -- literal value
  | LTopVar String                                 -- top-level name (fallback)
  | LApp    LExp LExp                              -- single-argument application
  | LLam    Variable LExp                          -- single-argument lambda
  | LFix    [(Variable, Variable, LExp)] LExp      -- mutually recursive functions
  | LTuple  [LExp]                                 -- n-tuple construction
  | LSelect Int LExp                               -- field selection (0-indexed)
  | LCon    String LExp                            -- constructor application (with payload)
  | LDecon  String LExp                            -- strip constructor, get payload
  | LSwitch LExp [(ConAlt, LExp)] (Maybe LExp)     -- single-level constructor test
  | LPrim   PrimOp [LExp]                          -- saturated or partial primitive call
  deriving (Show)
