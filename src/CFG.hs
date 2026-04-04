{-# LANGUAGE StandaloneDeriving #-}
module CFG
  ( IRType(..)
  , IRAtom(..)
  , IRBinding(..)
  , IRStmt(..)
  , IRWhileUpdate(..)
  , IRParam(..)
  , IRFunction(..)
  , IROutputLayout(..)
  , CFGModule(..)
  ) where

import GateDef (CallableKind)
import LambdaIR (PrimOp)


-- | Types of values in the CFG IR.
data IRType
  = IRQubit          -- a concrete qubit slot (q[i])
  | IRInt            -- int[32]
  | IRFloat          -- float[64]
  | IRBool           -- bool (result of comparison / PNot)
  | IRBit            -- bit (result of PMeas)
  | IRUnit           -- unit value; parameters of this type are dropped at codegen
  | IRRecord [IRType]-- residual record surviving RecordFlatten
  | IRLabel          -- defunctionalized integer dispatch tag
  deriving (Eq, Show)


-- | Atomic values — no computation, just a reference or constant.
data IRAtom
  = IRVar       String     -- local or parameter variable
  | IRQubitSlot Int        -- q[i]
  | IRIntConst  Int
  | IRFloatConst String
  | IRBitConst  Bool
  | IRUnitVal
  deriving (Eq, Show)


-- | Right-hand side of a let binding.
data IRBinding
  = IRBindPrim   PrimOp [IRAtom]   -- result of a classical primitive
  | IRBindMeas   IRAtom            -- bit <- measure q[i]
  | IRBindSelect Int IRAtom        -- field i of a record atom
  | IRBindOffset Int IRAtom        -- interior pointer into a record
  | IRBindRecord [IRAtom]          -- record construction
  deriving (Eq, Show)


-- | Statements in a CFG function body.
--
-- Bodies are sequences of IRStmt.  The last statement in a function body is
-- always either an IRTailCall or a control-flow statement (IRIf, IRSwitch,
-- IRWhile) whose every arm eventually terminates with an IRTailCall.
data IRStmt
  = IRLet      String IRType IRBinding
    -- ^ Declare and initialise a new local: let name : type = binding.
  | IRAssign   String IRAtom
    -- ^ Assign to an already-declared mutable variable (used inside while
    --   loops to update classical parameters on each self-recursive call).
  | IRGate     String [IRAtom]
    -- ^ Apply a quantum gate: h q[0]; cx q[0], q[1]; etc.
  | IRTailCall String [IRAtom]
    -- ^ Direct tail call to a named function (or "halt" for exit).
  | IRIf       IRAtom [IRStmt] [IRStmt]
    -- ^ Structured two-arm conditional.
  | IRSwitch   IRAtom [(Int, [IRStmt])]
    -- ^ Multi-way switch on an integer dispatch tag (from defunctionalization).
  | IRWhile    IRAtom [IRStmt]
    -- ^ Compiled self-recursive tail loop.
    --   Condition atom: the loop-running flag (e.g. IRVar "done").
    --   Body statements: the loop body, including IRAssign updates at
    --   self-recursive call sites and a flag-set at the exit site.
  deriving (Eq, Show)


-- | A classical parameter update emitted inside a while loop body.
--
-- Used by compileBodyAsLoop in ToCFG to describe which mutable variables
-- are updated on each self-recursive call.  The renderer emits these as
-- IRAssign statements at self-call sites.
data IRWhileUpdate = IRWhileUpdate String IRAtom
  deriving (Eq, Show)


-- | A typed function parameter.
data IRParam = IRParam
  { irParamName :: String
  , irParamType :: IRType
  } deriving (Eq, Show)


-- | A lifted function definition in the CFG IR.
data IRFunction = IRFunction
  { irFuncName   :: String
  , irFuncKind   :: CallableKind   -- gate or def (from GateDef.hs)
  , irFuncParams :: [IRParam]      -- IRUnit params excluded
  , irFuncBody   :: [IRStmt]
  , irFuncIsLoop :: Bool           -- True => body contains an IRWhile
  }

deriving instance Show IRFunction


-- | Output layout for the entry-point function.
data IROutputLayout
  = IROutputScalars [IRType]
  | IROutputArray   IRType Int
  deriving (Eq, Show)


-- | A compiled FunQ module ready for QASM emission.
data CFGModule = CFGModule
  { cfgFunctions    :: [IRFunction]
  , cfgQubitCount   :: Int
  , cfgEntryPoint   :: String        -- typically "output"
  , cfgOutputLayout :: IROutputLayout
  } deriving (Show)
