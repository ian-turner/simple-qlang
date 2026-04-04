-- | Lower the pre-ClosureConv CPS (with qubit slots hoisted) to the CFG IR.
--
-- Input: a CompiledModule.  For each declaration we use compiledInterfaceIR
-- (post-ModuleRecordFlatten, pre-ClosureConv) and apply hoistQubits to it to
-- get concrete VQubit slots.
--
-- The pre-ClosureConv CPS may have:
--   - Nested CFix groups inside function bodies (local continuations).
--   - CApp (VVar f) calls to CFix-bound variables (both self-recursive calls
--     and calls to local continuation functions).
--   - CApp (VLabel "halt") for the program exit.
--   - VQubit i slots (after hoistQubits).
--
-- Nested CFix groups are handled by inlining their bodies at each call site
-- (stored in fcLocalFuncs).  Self-recursive tail calls are compiled as
-- IRWhile loops.
--
-- Output: a CFGModule with one IRFunction per top-level CPS function, ready
-- for QASM emission.
module ToCFG (toCFGModule) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (forM)
import Control.Monad.State.Strict

import CFG
import qualified CPSAtom as A
import CPSExp
import CompilePipeline
import GateDef (CallableKind(..), ModuleCallableKinds(..), lookupTopLevelCallableKind)
import LambdaIR (PrimOp(..))
import QubitHoist (HoistedProgram(..), hoistQubits)
import Utils (Variable)


-- ---------------------------------------------------------------------------
-- Lowering monad (fresh name counters)
-- ---------------------------------------------------------------------------

data LowerState = LowerState
  { lsNextBit  :: Int
  , lsNextTemp :: Int
  }

type LowerM = StateT LowerState (Either String)


freshBitName :: LowerM String
freshBitName = do
  n <- gets lsNextBit
  modify (\s -> s { lsNextBit = n + 1 })
  pure ("m" ++ show n)


freshTempName :: LowerM String
freshTempName = do
  n <- gets lsNextTemp
  modify (\s -> s { lsNextTemp = n + 1 })
  pure ("t" ++ show n)


-- ---------------------------------------------------------------------------
-- Variable naming
-- ---------------------------------------------------------------------------

-- | Convert a Variable to a stable QASM identifier string.
-- Prefixed with "v" to avoid clashes with QASM keywords and generated names.
varName :: Variable -> String
varName v = "v" ++ show v



-- ---------------------------------------------------------------------------
-- Step 3b: Parameter type inference
-- ---------------------------------------------------------------------------

-- | Infer the IRType for each Variable that appears as a function parameter
-- across all functions in the CFix group.
--
-- Strategy: scan use sites in all function bodies.  Variables used as qubit
-- arguments to gates/measurements get IRQubit; results of PMeas get IRBit;
-- results of boolean primitives get IRBool; all others default to IRInt.
inferParamTypes
  :: [(Variable, [Variable], CExp)]
  -> Map.Map Variable IRType
inferParamTypes defs =
  let allParams = Set.fromList (concatMap (\(_, ps, _) -> ps) defs)
      hints =
        Map.unionsWith mergeHint
          (map (\(_, _, body) -> collectHints body) defs)
  in Map.fromList
       [ (v, Map.findWithDefault IRInt v hints)
       | v <- Set.toList allParams
       ]


-- | Merge two type hints: more specific types win.
-- Priority: IRQubit > IRBit > IRBool > IRInt > IRLabel > IRUnit > IRRecord
mergeHint :: IRType -> IRType -> IRType
mergeHint IRQubit _ = IRQubit
mergeHint _ IRQubit = IRQubit
mergeHint IRBit _ = IRBit
mergeHint _ IRBit = IRBit
mergeHint IRBool _ = IRBool
mergeHint _ IRBool = IRBool
mergeHint IRInt _ = IRInt
mergeHint _ IRInt = IRInt
mergeHint a _ = a


-- | Collect type hints by scanning a single CPS expression.
collectHints :: CExp -> Map.Map Variable IRType
collectHints expr = go Map.empty expr
  where
    go acc (CRecord _ _ body) = go acc body
    go acc (CSelect _ _ _ body) = go acc body
    go acc (COffset _ _ _ body) = go acc body
    go acc (CApp _ _) = acc
    go acc (CFix defs body) =
      -- Process defs then body, left-folding the accumulator.
      let acc1 = go acc body
      in foldl (\a (_, _, b) -> go a b) acc1 defs
    go acc (CSwitch (VVar v) arms) =
      -- Scrutinee is used as an integer dispatch tag.
      foldl go (Map.insertWith mergeHint v IRLabel acc) arms
    go acc (CSwitch _ arms) = foldl go acc arms
    go acc (CPrimOp op args results conts) =
      let acc1 = foldl go acc conts
          -- Hints from result variables.
          acc2 = case (op, results) of
            (PMeas, [r]) ->
              Map.insertWith mergeHint r IRBit acc1
            (PHGate, rs) -> markVars IRQubit rs acc1
            (PXGate, rs) -> markVars IRQubit rs acc1
            (PZGate, rs) -> markVars IRQubit rs acc1
            (PSGate, rs) -> markVars IRQubit rs acc1
            (PTGate, rs) -> markVars IRQubit rs acc1
            (PCNot, rs) -> markVars IRQubit rs acc1
            (PCSGate, rs) -> markVars IRQubit rs acc1
            (PCTGate, rs) -> markVars IRQubit rs acc1
            (PCpGate, rs) -> markVars IRQubit rs acc1
            _ | isBoolPrim op, [r] <- results ->
                Map.insertWith mergeHint r IRBool acc1
            _ -> acc1
          -- Hints from argument variables.
          acc3 = case op of
            PMeas -> markArgVars IRQubit args acc2
            PHGate -> markArgVars IRQubit args acc2
            PXGate -> markArgVars IRQubit args acc2
            PZGate -> markArgVars IRQubit args acc2
            PSGate -> markArgVars IRQubit args acc2
            PTGate -> markArgVars IRQubit args acc2
            PCNot -> markArgVars IRQubit args acc2
            PCSGate -> markArgVars IRQubit args acc2
            PCTGate -> markArgVars IRQubit args acc2
            PCpGate ->
              case args of
                _ : q0 : q1 : _ -> markArgVars IRQubit [q0, q1] acc2
                _ -> acc2
            _ -> acc2
      in acc3
    markVars ty vs acc =
      foldr (\v a -> Map.insertWith mergeHint v ty a) acc vs
    markArgVars ty vals acc =
      foldr
        ( \v a ->
            case v of
              VVar x -> Map.insertWith mergeHint x ty a
              _ -> a
        )
        acc
        vals


-- ---------------------------------------------------------------------------
-- Lowering context
-- ---------------------------------------------------------------------------

-- | Context threaded through lowerBody.
data FuncCtx = FuncCtx
  { fcTypeEnv    :: Map.Map Variable IRType
    -- ^ Types of all variables in scope (params + local bindings).
  , fcAtomEnv    :: A.Env
    -- ^ CPSAtom constant-folding environment: maps variables to known atoms.
  , fcNameEnv    :: Map.Map Variable String
    -- ^ Maps variables to their generated QASM names (e.g. bit names from PMeas).
  , fcSelfName   :: String
    -- ^ String name of the function currently being lowered (for self-call detection).
  , fcLoopCtx    :: Maybe LoopCtx
    -- ^ Non-Nothing when compiling this function as a while loop.
  , fcAllNames   :: Map.Map Variable String
    -- ^ All CFix function names: Variable → String.
  , fcLocalFuncs :: Map.Map Variable ([Variable], CExp)
    -- ^ Local continuation functions from nested CFix groups (for inlining).
    -- When lowerBody encounters CFix, it adds the defs here so that CApp
    -- (VVar f) calls can inline them rather than emitting undefined IRTailCalls.
  }


-- | Context for while-loop compilation of a self-recursive function.
data LoopCtx = LoopCtx
  { lcParams  :: [Variable]
    -- ^ Original parameter list (positional, matching CApp arg order).
  , lcMutVars :: Map.Map Variable String
    -- ^ Classical parameters → names of their mutable temp variables.
  , lcDoneVar :: String
    -- ^ Name of the boolean loop-running flag.
  }


-- ---------------------------------------------------------------------------
-- Value/atom lowering helpers
-- ---------------------------------------------------------------------------

-- | Lower a CPS Value to an IRAtom, consulting the atom and name environments
-- for constant-folded and generated-name bindings.
lowerVal :: FuncCtx -> Value -> IRAtom
lowerVal ctx (VVar v) =
  -- First check if this variable has a generated QASM name (e.g. from PMeas).
  case Map.lookup v (fcNameEnv ctx) of
    Just name -> IRVar name
    Nothing ->
      -- Then try the CPSAtom constant-folding environment.
      case A.atomOfValue (fcAtomEnv ctx) (VVar v) of
        Just (A.AScalar (VVar v')) | v' /= v ->
          lowerVal ctx (VVar v')
        Just (A.AScalar (VVar _)) ->
          -- No substitution available (atom is the variable itself).
          IRVar (stableName ctx v)
        Just (A.AScalar val') ->
          lowerVal ctx val'
        _ ->
          IRVar (stableName ctx v)
  where
    -- Use the pre-built name map (which covers both functions and params)
    -- before falling back to the generic varName.
    stableName c var = Map.findWithDefault (varName var) var (fcAllNames c)
lowerVal _ (VQubit i) = IRQubitSlot i
lowerVal _ (VInt n) = IRIntConst n
lowerVal _ (VFloat s) = IRFloatConst s
lowerVal _ VUnit = IRUnitVal
lowerVal _ (VLabel l) = IRVar l


-- | Look up the type of a variable.
varType :: FuncCtx -> Variable -> IRType
varType ctx v = Map.findWithDefault IRInt v (fcTypeEnv ctx)


-- | Extend the atom environment.
withAtom :: Variable -> A.Atom -> FuncCtx -> FuncCtx
withAtom v a ctx = ctx { fcAtomEnv = Map.insert v a (fcAtomEnv ctx) }


-- | Extend the type environment.
withType :: Variable -> IRType -> FuncCtx -> FuncCtx
withType v ty ctx = ctx { fcTypeEnv = Map.insert v ty (fcTypeEnv ctx) }


-- | Extend the name environment.
withName :: Variable -> String -> FuncCtx -> FuncCtx
withName v name ctx = ctx { fcNameEnv = Map.insert v name (fcNameEnv ctx) }


-- | Add local continuation functions from a nested CFix group to the context.
withLocalFuncs :: [(Variable, [Variable], CExp)] -> FuncCtx -> FuncCtx
withLocalFuncs defs ctx =
  ctx { fcLocalFuncs = Map.union newEntries (fcLocalFuncs ctx) }
  where
    newEntries = Map.fromList [(v, (ps, b)) | (v, ps, b) <- defs]


-- ---------------------------------------------------------------------------
-- Steps 4 & 5: CPS body lowering
-- ---------------------------------------------------------------------------

-- | Translate a CPS expression to a list of IRStmt.
lowerBody :: FuncCtx -> CExp -> LowerM [IRStmt]

-- CRecord: constant-fold if all fields are statically known; otherwise emit.
lowerBody ctx (CRecord fields v body) =
  case A.recordAtom (fcAtomEnv ctx) fields of
    Just atom ->
      lowerBody (withAtom v atom ctx) body
    Nothing -> do
      tmpName <- freshTempName
      let atoms = map (lowerVal ctx . fst) fields
          ctx' = withName v tmpName (withType v (IRRecord []) ctx)
      rest <- lowerBody ctx' body
      pure (IRLet tmpName (IRRecord []) (IRBindRecord atoms) : rest)

-- CSelect: constant-fold if the record is statically known; otherwise emit.
lowerBody ctx (CSelect i val v body) =
  case A.selectAtom (fcAtomEnv ctx) val i of
    Just atom ->
      lowerBody (withAtom v atom ctx) body
    Nothing -> do
      tmpName <- freshTempName
      let ty = varType ctx v
          arg = lowerVal ctx val
          ctx' = withName v tmpName (withType v ty ctx)
      rest <- lowerBody ctx' body
      pure (IRLet tmpName ty (IRBindSelect i arg) : rest)

-- COffset: constant-fold if known; otherwise emit.
lowerBody ctx (COffset off val v body) =
  case A.offsetAtom (fcAtomEnv ctx) val off of
    Just atom ->
      lowerBody (withAtom v atom ctx) body
    Nothing -> do
      tmpName <- freshTempName
      let arg = lowerVal ctx val
          ctx' = withName v tmpName (withType v (IRRecord []) ctx)
      rest <- lowerBody ctx' body
      pure (IRLet tmpName (IRRecord []) (IRBindOffset off arg) : rest)

-- CApp (VLabel "halt"): function exit.
lowerBody ctx (CApp (VLabel "halt") args) = do
  let atoms = map (lowerVal ctx) args
  case fcLoopCtx ctx of
    Just lctx ->
      -- Inside a while loop: set the done flag, then record the halt call.
      pure
        [ IRAssign (lcDoneVar lctx) (IRBitConst True)
        , IRTailCall "halt" atoms
        ]
    Nothing ->
      pure [IRTailCall "halt" atoms]

-- CApp (VLabel l): direct tail call.
-- Strip the trailing continuation argument (CPS convention: last arg is always
-- the continuation for top-level function calls).
lowerBody ctx (CApp (VLabel l) args) = do
  let coreArgs = stripContArg ctx args
      atoms = map (lowerVal ctx) coreArgs
  case fcLoopCtx ctx of
    Just lctx | l == fcSelfName ctx ->
      -- Self-call in while-loop mode: emit parameter updates and continue.
      pure (buildLoopUpdates lctx (fcSelfName ctx) (zip (lcParams lctx) coreArgs) ctx)
    _ ->
      pure [IRTailCall l atoms]

-- CApp (VVar f): call to a locally-bound variable.
-- In pre-ClosureConv CPS, this is either:
--   (a) a call to a local continuation in fcLocalFuncs — inline it, or
--   (b) a variable bound in the atom env (e.g. the continuation pre-bound to
--       "halt") — resolve and redirect, or
--   (c) a self-recursive tail call (handled as a while-loop update), or
--   (d) a call to another top-level function (emit as IRTailCall).
lowerBody ctx (CApp (VVar f) args) =
  case Map.lookup f (fcLocalFuncs ctx) of
    Just (params, funcBody) ->
      -- Inline: bind params to args via the atom env, then lower the body.
      let ctx' = foldl (\c (p, v) -> withAtom p (A.AScalar v) c) ctx (zip params args)
      in lowerBody ctx' funcBody
    Nothing ->
      -- Check the atom env: the continuation parameter may be pre-bound to
      -- a label (e.g. VLabel "halt").
      case Map.lookup f (fcAtomEnv ctx) of
        Just (A.AScalar (VLabel l)) ->
          lowerBody ctx (CApp (VLabel l) args)
        Just (A.AScalar (VVar f')) | f' /= f ->
          lowerBody ctx (CApp (VVar f') args)
        _ -> do
          let coreArgs = stripContArg ctx args
              atoms = map (lowerVal ctx) coreArgs
              name = Map.findWithDefault (varName f) f (fcAllNames ctx)
          case fcLoopCtx ctx of
            Just lctx | name == fcSelfName ctx ->
              pure (buildLoopUpdates lctx name (zip (lcParams lctx) coreArgs) ctx)
            _ ->
              pure [IRTailCall name atoms]

-- CApp with any other callee form (VInt, VFloat, VQubit, VUnit): not expected.
lowerBody _ (CApp _ _) =
  lift (Left "ToCFG.lowerBody: CApp with unexpected callee form")

-- CFix: nested local continuation functions.  Add them to the context for
-- inlining, then lower the body expression.
lowerBody ctx (CFix defs body) =
  lowerBody (withLocalFuncs defs ctx) body

-- CSwitch: branch.
lowerBody ctx (CSwitch val arms) = do
  let scrAtom = lowerVal ctx val
  case scrAtom of
    IRIntConst tag ->
      -- Constant-fold: only lower the matching arm.
      if tag >= 0 && tag < length arms
        then lowerBody ctx (arms !! tag)
        else
          lift
            ( Left
                ( "ToCFG: CSwitch constant tag "
                    ++ show tag
                    ++ " out of range (have "
                    ++ show (length arms)
                    ++ " arms)"
                )
            )
    _ ->
      case arms of
        [arm0, arm1] -> do
          s0 <- lowerBody ctx arm0
          s1 <- lowerBody ctx arm1
          -- CSwitch arm0 is the case-0 (falsy) branch, arm1 is the case-1 (truthy)
          -- branch.  IRIf cond then else means "if cond is truthy then then else
          -- else", so the truthy case (arm1) becomes the then branch.
          pure [IRIf scrAtom s1 s0]
        _ -> do
          armStmts <- mapM (lowerBody ctx) arms
          pure [IRSwitch scrAtom (zip [0 ..] armStmts)]

-- CPrimOp: quantum gates and classical ops.
lowerBody ctx (CPrimOp op args results [cont]) =
  lowerPrimOp ctx op args results cont
lowerBody _ (CPrimOp _ _ _ conts) =
  lift
    ( Left
        ( "ToCFG: CPrimOp with "
            ++ show (length conts)
            ++ " continuations (expected 1)"
        )
    )


-- | Strip the trailing continuation argument from a CPS call's argument list.
--
-- In pre-ClosureConv CPS every call to a top-level function ends with a
-- continuation as the last argument.  The CFG IR does not carry explicit
-- continuations, so we strip the last arg when it is recognisably a
-- continuation:
--   • VVar v where v is in fcLocalFuncs (a local continuation wrapper), or
--   • VVar v where v is bound in fcAtomEnv to a VLabel (e.g. the continuation
--     pre-bound to "halt"), or
--   • VLabel _ (a direct label used as a continuation).
--
-- If the last arg does not look like a continuation it is left in place.
stripContArg :: FuncCtx -> [Value] -> [Value]
stripContArg _ [] = []
stripContArg ctx args =
  case last args of
    VVar v
      | Map.member v (fcLocalFuncs ctx)                               -> init args
      | Just (A.AScalar (VLabel _)) <- Map.lookup v (fcAtomEnv ctx)  -> init args
    VLabel _ -> init args
    _         -> args


-- | Build the list of IRStmt for a while-loop self-recursive call.
-- Emits IRAssign statements for classical params that receive new values.
buildLoopUpdates
  :: LoopCtx
  -> String
  -> [(Variable, Value)]  -- zip of params with new arg values
  -> FuncCtx
  -> [IRStmt]
buildLoopUpdates lctx _name paramArgPairs ctx =
  [ IRAssign mutVar (lowerVal ctx arg)
  | (param, arg) <- paramArgPairs
  , Just mutVar <- [Map.lookup param (lcMutVars lctx)]
  ]
  -- No explicit "continue" needed: the while condition is rechecked naturally.


-- ---------------------------------------------------------------------------
-- Step 4: Primitive operation lowering
-- ---------------------------------------------------------------------------

-- | Lower a CPrimOp to a list of IRStmt (prepended to the lowered continuation).
lowerPrimOp
  :: FuncCtx
  -> PrimOp
  -> [Value]
  -> [Variable]
  -> CExp
  -> LowerM [IRStmt]

-- PInit must have been replaced by QubitHoist.
lowerPrimOp _ PInit _ _ _ =
  lift (Left "ToCFG: PInit unexpectedly survived QubitHoist")

-- Gate-as-value: a gate primop with NO qubit arguments produces a gate
-- reference (first-class gate value for higher-order quantum).  Represent
-- the reference as an integer tag matching the defunctionalization convention
-- used elsewhere in the pipeline.
lowerPrimOp ctx PHGate [] [result] cont = do
  let ctx' = withAtom result (A.AScalar (VInt 0)) (withType result IRInt ctx)
  lowerBody ctx' cont
lowerPrimOp ctx PXGate [] [result] cont = do
  let ctx' = withAtom result (A.AScalar (VInt 1)) (withType result IRInt ctx)
  lowerBody ctx' cont
lowerPrimOp ctx PZGate [] [result] cont = do
  let ctx' = withAtom result (A.AScalar (VInt 2)) (withType result IRInt ctx)
  lowerBody ctx' cont
lowerPrimOp ctx PSGate [] [result] cont = do
  let ctx' = withAtom result (A.AScalar (VInt 3)) (withType result IRInt ctx)
  lowerBody ctx' cont
lowerPrimOp ctx PTGate [] [result] cont = do
  let ctx' = withAtom result (A.AScalar (VInt 4)) (withType result IRInt ctx)
  lowerBody ctx' cont

-- Single-qubit gates.
-- The result variable is an alias for the input qubit (gates modify in-place);
-- bind it in the atom env so subsequent references resolve to the input atom.
lowerPrimOp ctx PHGate [arg] [result] cont = do
  let ctx' = withAtom result (A.AScalar arg) (withType result IRQubit ctx)
  rest <- lowerBody ctx' cont
  pure (IRGate "h" [lowerVal ctx arg] : rest)

lowerPrimOp ctx PXGate [arg] [result] cont = do
  let ctx' = withAtom result (A.AScalar arg) (withType result IRQubit ctx)
  rest <- lowerBody ctx' cont
  pure (IRGate "x" [lowerVal ctx arg] : rest)

lowerPrimOp ctx PZGate [arg] [result] cont = do
  let ctx' = withAtom result (A.AScalar arg) (withType result IRQubit ctx)
  rest <- lowerBody ctx' cont
  pure (IRGate "z" [lowerVal ctx arg] : rest)

lowerPrimOp ctx PSGate [arg] [result] cont = do
  let ctx' = withAtom result (A.AScalar arg) (withType result IRQubit ctx)
  rest <- lowerBody ctx' cont
  pure (IRGate "s" [lowerVal ctx arg] : rest)

lowerPrimOp ctx PTGate [arg] [result] cont = do
  let ctx' = withAtom result (A.AScalar arg) (withType result IRQubit ctx)
  rest <- lowerBody ctx' cont
  pure (IRGate "t" [lowerVal ctx arg] : rest)

-- Two-qubit gates.
-- Both result variables alias their respective input qubits.
lowerPrimOp ctx PCNot [a0, a1] [r0, r1] cont = do
  let ctx' = withAtom r0 (A.AScalar a0) (withAtom r1 (A.AScalar a1)
               (withType r0 IRQubit (withType r1 IRQubit ctx)))
  rest <- lowerBody ctx' cont
  pure (IRGate "cx" [lowerVal ctx a0, lowerVal ctx a1] : rest)

lowerPrimOp ctx PCSGate [a0, a1] [r0, r1] cont = do
  let ctx' = withAtom r0 (A.AScalar a0) (withAtom r1 (A.AScalar a1)
               (withType r0 IRQubit (withType r1 IRQubit ctx)))
  rest <- lowerBody ctx' cont
  pure (IRGate "cp(pi/2)" [lowerVal ctx a0, lowerVal ctx a1] : rest)

lowerPrimOp ctx PCTGate [a0, a1] [r0, r1] cont = do
  let ctx' = withAtom r0 (A.AScalar a0) (withAtom r1 (A.AScalar a1)
               (withType r0 IRQubit (withType r1 IRQubit ctx)))
  rest <- lowerBody ctx' cont
  pure (IRGate "cp(pi/4)" [lowerVal ctx a0, lowerVal ctx a1] : rest)

lowerPrimOp ctx PCpGate [kArg, a0, a1] [r0, r1] cont = do
  let k = lowerVal ctx kArg
      gateName = case k of
        IRIntConst n -> "cp(pi/" ++ show (2 ^ n :: Int) ++ ")"
        _ -> "cp_param"
      ctx' = withAtom r0 (A.AScalar a0) (withAtom r1 (A.AScalar a1)
               (withType r0 IRQubit (withType r1 IRQubit ctx)))
  rest <- lowerBody ctx' cont
  pure (IRGate gateName [lowerVal ctx a0, lowerVal ctx a1] : rest)

-- Measurement.
lowerPrimOp ctx PMeas [arg] [result] cont = do
  bitName <- freshBitName
  let ctx' = withName result bitName (withType result IRBit ctx)
  rest <- lowerBody ctx' cont
  pure (IRLet bitName IRBit (IRBindMeas (lowerVal ctx arg)) : rest)

-- Classical primitives.
lowerPrimOp ctx op args [result] cont
  | isClassPrim op = do
      let argAtoms = map (lowerVal ctx) args
          resultTy = inferPrimType op argAtoms
      case evalConstPrim op argAtoms of
        Just constVal ->
          -- Constant-fold: bind result to the constant, no statement.
          lowerBody
            ( withAtom result (A.AScalar (VInt constVal))
                (withType result IRInt ctx)
            )
            cont
        Nothing -> do
          tempName <- freshTempName
          let ctx' = withName result tempName (withType result resultTy ctx)
          rest <- lowerBody ctx' cont
          pure (IRLet tempName resultTy (IRBindPrim op argAtoms) : rest)

lowerPrimOp _ op args results _ =
  lift
    ( Left
        ( "ToCFG: unsupported primop form: "
            ++ show op
            ++ " with "
            ++ show (length args)
            ++ " args, "
            ++ show (length results)
            ++ " results"
        )
    )


-- ---------------------------------------------------------------------------
-- Primitive op helpers
-- ---------------------------------------------------------------------------

isBoolPrim :: PrimOp -> Bool
isBoolPrim op = op `elem` [PEq, PLt, PGt, PLe, PGe, PAnd, POr, PNot]


isClassPrim :: PrimOp -> Bool
isClassPrim op =
  op `elem` [PAdd, PSub, PMul, PDiv, PEq, PLt, PGt, PLe, PGe, PAnd, POr, PNot]


-- | Infer the result type of a classical primitive from its argument atoms.
inferPrimType :: PrimOp -> [IRAtom] -> IRType
inferPrimType op _
  | isBoolPrim op = IRBool
  | any isFloatAtom [] = IRFloat
  | otherwise = IRInt
  where
    isFloatAtom (IRFloatConst _) = True
    isFloatAtom _ = False


-- | Evaluate a classical primitive on constant integer atom arguments.
evalConstPrim :: PrimOp -> [IRAtom] -> Maybe Int
evalConstPrim op atoms =
  mapM asInt atoms >>= eval op
  where
    asInt (IRIntConst n) = Just n
    asInt (IRBitConst b) = Just (if b then 1 else 0)
    asInt _ = Nothing

    b2i b = if b then 1 else 0

    eval PAdd [a, b] = Just (a + b)
    eval PSub [a, b] = Just (a - b)
    eval PMul [a, b] = Just (a * b)
    eval PDiv [a, b]
      | b /= 0 = Just (a `div` b)
    eval PEq [a, b] = Just (b2i (a == b))
    eval PLt [a, b] = Just (b2i (a < b))
    eval PGt [a, b] = Just (b2i (a > b))
    eval PLe [a, b] = Just (b2i (a <= b))
    eval PGe [a, b] = Just (b2i (a >= b))
    eval PAnd [a, b] = Just (b2i (a /= 0 && b /= 0))
    eval POr [a, b] = Just (b2i (a /= 0 || b /= 0))
    eval PNot [a] = Just (b2i (a == 0))
    eval _ _ = Nothing


-- ---------------------------------------------------------------------------
-- Step 6: Tail-loop detection (ported from OpenQASM.hs)
-- ---------------------------------------------------------------------------

-- | True if every self-recursive call passes the continuation through
-- unchanged (directly or via an η-trivial chain).  If so, the function can
-- be compiled as an OpenQASM while loop.
--
-- In pre-ClosureConv CPS, self-recursive calls appear as CApp (VVar funcVar)
-- (not CApp (VLabel name)), so we check both forms.
isTailLoop :: String -> Variable -> [Variable] -> CExp -> Bool
isTailLoop name funcVar params body =
  case params of
    [] -> False
    _ ->
      let contParam = last params
          calls = findSelfCalls Map.empty name funcVar body
      in not (null calls) && all (contIsUnchanged contParam) calls
  where
    findSelfCalls localDefs n fv (CRecord _ _ b) =
      findSelfCalls localDefs n fv b
    findSelfCalls localDefs n fv (CSelect _ _ _ b) =
      findSelfCalls localDefs n fv b
    findSelfCalls localDefs n fv (COffset _ _ _ b) =
      findSelfCalls localDefs n fv b
    findSelfCalls localDefs n _ (CApp (VLabel l) args)
      | l == n = [(localDefs, args)]
    findSelfCalls localDefs _ fv (CApp (VVar v) args)
      | v == fv = [(localDefs, args)]
    findSelfCalls _ _ _ (CApp _ _) = []
    findSelfCalls localDefs n fv (CFix defs b) =
      let localDefs' =
            Map.union
              (Map.fromList [(f, (ps, fb)) | (f, ps, fb) <- defs])
              localDefs
      in concatMap (\(_, _, db) -> findSelfCalls localDefs' n fv db) defs
           ++ findSelfCalls localDefs' n fv b
    findSelfCalls localDefs n fv (CSwitch _ arms) =
      concatMap (findSelfCalls localDefs n fv) arms
    findSelfCalls localDefs n fv (CPrimOp _ _ _ conts) =
      concatMap (findSelfCalls localDefs n fv) conts

    contIsUnchanged contParam (localDefs, args) =
      case reverse args of
        (VVar v : _) -> reaches Set.empty localDefs v
        _ -> False
      where
        reaches _ _ v | v == contParam = True
        reaches visited defs v
          | Set.member v visited = False  -- cycle: not an η-trivial chain
          | otherwise =
              case Map.lookup v defs of
                Nothing -> True
                Just (ps, CApp (VVar cont) appArgs)
                  | appArgs == map VVar ps ->
                      reaches (Set.insert v visited) defs cont
                Just ([x], CApp (VVar appFun) appArgs)
                  | appFun == x
                  , not (null appArgs)
                  , VVar lastArg <- last appArgs ->
                      reaches (Set.insert v visited) defs lastArg
                _ -> False


-- | Compile a self-recursive tail-loop function body to a while loop.
--
-- Classical parameters receive mutable temp variables declared before the
-- loop.  Qubit parameters are invariant across iterations.  A boolean done
-- flag controls loop exit.
compileBodyAsLoop
  :: FuncCtx
  -> String          -- function name
  -> [Variable]      -- parameter list
  -> CExp            -- function body
  -> LowerM [IRStmt]
compileBodyAsLoop ctx name params body = do
  -- Identify classical parameters (those not typed as qubit/unit/record).
  let typedParams = [(p, Map.findWithDefault IRInt p (fcTypeEnv ctx)) | p <- params]
      classicParams = [(p, ty) | (p, ty) <- typedParams, isClassicType ty]

  -- Allocate mutable temp vars for classical parameters.
  classicTriples <- forM classicParams $ \(p, ty) -> do
    mutVar <- freshTempName
    pure (p, ty, mutVar)

  -- Allocate the loop-done flag.
  doneVar <- freshTempName

  -- Build the initial context for the loop body:
  --   - classical params refer to their mutable vars
  --   - qubit params keep their original varName
  let mutVarMap = Map.fromList [(p, mv) | (p, _, mv) <- classicTriples]
      loopCtx = LoopCtx
        { lcParams = params
        , lcMutVars = mutVarMap
        , lcDoneVar = doneVar
        }
      ctx' =
        ctx
          { fcSelfName = name
          , fcLoopCtx = Just loopCtx
          , fcNameEnv =
              Map.union (Map.fromList [(p, mv) | (p, _, mv) <- classicTriples]) (fcNameEnv ctx)
          }

  -- Lower the body with loop context.
  bodyStmts <- lowerBody ctx' body

  -- Build init statements (declare done flag + mutable classical vars).
  -- We need the initial values: they come from the params themselves (the
  -- mutable var starts equal to the param).
  let doneDecl = IRLet doneVar IRBool (IRBindPrim PNot [IRIntConst 1])
      -- IRBindPrim PNot [1] = false: the loop is not done yet.
      classicDecls =
        [ IRLet mutVar ty (IRBindPrim PAdd [IRVar paramName, IRIntConst 0])
          -- IRBindPrim PAdd [p, 0] = p (copy the initial param value).
        | (p, ty, mutVar) <- classicTriples
        , let paramName = Map.findWithDefault (varName p) p (fcAllNames ctx)
        ]

  -- The condition is "not done".
  let condAtom = IRVar doneVar

  pure (doneDecl : classicDecls ++ [IRWhile condAtom bodyStmts])
  where
    isClassicType IRQubit = False
    isClassicType IRUnit = False
    isClassicType (IRRecord _) = False
    isClassicType _ = True


-- ---------------------------------------------------------------------------
-- Step 7: Per-function lowering and module assembly
-- ---------------------------------------------------------------------------

-- | Lower a single CFix function to an IRFunction.
lowerFunction
  :: String                    -- declaration name (for the exported function)
  -> Variable                  -- the exported function variable (from extractCFix)
  -> ModuleCallableKinds       -- gate/def classification
  -> Map.Map Variable String   -- all CFix function Variable → String name
  -> Map.Map Variable IRType   -- inferred parameter types
  -> (Variable, [Variable], CExp)
  -> LowerM IRFunction
lowerFunction _declName exportedFn callableKinds allNames typeEnv (funcVar, params, body) = do
  let name = Map.findWithDefault (varName funcVar) funcVar allNames
      kind = case lookupTopLevelCallableKind callableKinds name of
        Just k -> k
        Nothing -> CallableDef

      -- For the exported (top-level) function, the last parameter is the
      -- continuation, which is always called with the halt continuation.
      -- Pre-bind it to VLabel "halt" in the atom env and mark it IRUnit so
      -- it is excluded from the emitted parameter list.
      isExported = funcVar == exportedFn
      (contBindings, contTypeOverrides) =
        if isExported && not (null params)
          then
            let cp = last params
            in ( Map.singleton cp (A.AScalar (VLabel "halt"))
               , Map.singleton cp IRUnit
               )
          else (Map.empty, Map.empty)

      -- Build typed parameter list, dropping IRUnit params.
      typedParams =
        [ (p, Map.findWithDefault IRInt p (Map.union contTypeOverrides typeEnv))
        | p <- params
        ]
      irParams =
        [ IRParam (Map.findWithDefault (varName p) p allNames) ty
        | (p, ty) <- typedParams
        , ty /= IRUnit
        ]

      initCtx =
        FuncCtx
          { fcTypeEnv    = Map.fromList typedParams
          , fcAtomEnv    = contBindings
          , fcNameEnv    = Map.empty
          , fcSelfName   = name
          , fcLoopCtx    = Nothing
          , fcAllNames   = allNames
          , fcLocalFuncs = Map.empty
          }

  -- Check for tail-loop pattern (pass funcVar so VVar self-calls are detected).
  let isLoop = isTailLoop name funcVar params body
  bodyStmts <-
    if isLoop
      then compileBodyAsLoop initCtx name params body
      else lowerBody initCtx body

  pure
    IRFunction
      { irFuncName = name
      , irFuncKind = kind
      , irFuncParams = irParams
      , irFuncBody = bodyStmts
      , irFuncIsLoop = isLoop
      }



-- | Build the Variable → String name map for a CFix group.
-- The exported function (passed to halt) gets the declaration name;
-- all other variables (functions and parameters) get unique sequential names
-- like "v0", "v1", ... to avoid collisions from the nominal atom display.
buildNameMap
  :: String   -- declaration name (e.g. "output")
  -> Variable -- exported function variable
  -> [(Variable, [Variable], CExp)]
  -> Map.Map Variable String
buildNameMap declName exportedFn defs =
  let allVars = concatMap (\(fv, ps, _) -> fv : ps) defs
      (_, nameMap) = foldl step (0 :: Int, Map.empty) allVars
      step (n, m) v
        | Map.member v m = (n, m)
        | v == exportedFn = (n + 1, Map.insert v declName m)
        | otherwise = (n + 1, Map.insert v ("v" ++ show n) m)
  in nameMap


-- | Validate the compiled module and reject any declarations that failed.
rejectErrors :: CompiledModule -> Either String ()
rejectErrors cm =
  case
    [ err
    | Compiled decl <- compiledItems cm
    , Left err <- [compiledRecursionResult decl]
    ]
  of
    [] -> Right ()
    (err : _) -> Left err


-- | Find the compiled declaration for a given name.
findDecl :: String -> CompiledModule -> Maybe CompiledDecl
findDecl name cm =
  case
    [ decl
    | Compiled decl <- compiledItems cm
    , compiledName decl == name
    ]
  of
    (d : _) -> Just d
    [] -> Nothing


-- | Infer the output layout from the "output" function by finding an
-- IRTailCall "halt" node, following non-halt tail calls across functions.
--
-- The layout is determined by the first halt call found (all halt calls in a
-- well-formed program pass values of the same type/arity).
inferOutputLayout :: [IRFunction] -> IROutputLayout
inferOutputLayout funcs =
  case findHaltArgsFrom Set.empty "output" of
    Just types -> chooseLayout types
    Nothing -> IROutputScalars []
  where
    funcMap = Map.fromList [(irFuncName f, f) | f <- funcs]

    findHaltArgsFrom visited fname
      | Set.member fname visited = Nothing
      | otherwise =
          case Map.lookup fname funcMap of
            Nothing -> Nothing
            Just f -> findInStmts (Set.insert fname visited) (irFuncBody f)

    findInStmts _ [] = Nothing
    findInStmts _ (IRTailCall "halt" args : _) = Just (map atomType args)
    findInStmts visited (IRTailCall name _ : rest) =
      findHaltArgsFrom visited name <|> findInStmts visited rest
    findInStmts visited (IRIf _ t e : rest) =
      findInStmts visited t
        <|> findInStmts visited e
        <|> findInStmts visited rest
    findInStmts visited (IRWhile _ body : rest) =
      findInStmts visited body <|> findInStmts visited rest
    findInStmts visited (IRSwitch _ arms : rest) =
      foldr (\(_, ss) acc -> findInStmts visited ss <|> acc)
            (findInStmts visited rest) arms
    findInStmts visited (_ : rest) = findInStmts visited rest

    (<|>) (Just x) _ = Just x
    (<|>) Nothing y = y

    atomType (IRQubitSlot _) = IRQubit
    atomType (IRIntConst _) = IRInt
    atomType (IRFloatConst _) = IRFloat
    atomType (IRBitConst _) = IRBit
    atomType IRUnitVal = IRUnit
    atomType (IRVar _) = IRBit  -- conservative: assume bits (most common output)

    chooseLayout [] = IROutputScalars []
    chooseLayout [ty] = IROutputScalars [ty]
    chooseLayout tys
      | allSame tys, (ty : _) <- tys = IROutputArray ty (length tys)
      | otherwise = IROutputScalars tys

    allSame [] = True
    allSame (x : xs) = all (== x) xs


-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Lower a compiled module to the CFG IR.
toCFGModule :: CompiledModule -> Either String CFGModule
toCFGModule cm = do
  rejectErrors cm

  -- Collect (declName, hoisted interface IR) for each compiled declaration.
  -- We use compiledInterfaceIR (pre-ClosureConv) and run hoistQubits on it so
  -- that all VQubit slots are concrete.  The qubit count comes from the
  -- fully-compiled compiledHoistedIR (same count, ClosureConv/Defunc don't
  -- add qubits).
  let items =
        [ (compiledName decl, hoistQubits interfaceIR)
        | Compiled decl <- compiledItems cm
        , Just interfaceIR <- [compiledInterfaceIR decl]
        ]

  if null items
    then Left "ToCFG: no compiled declarations found"
    else Right ()

  -- Lower each declaration's CFix group to IRFunctions.
  (allFunctions, _finalState) <-
    runStateT (lowerDecls cm items) (LowerState 0 0)

  -- Determine the qubit count: use the output declaration's hoisted count
  -- from the fully-compiled pipeline (same as what hoistQubits would give).
  let qubitCount = case findDecl "output" cm >>= compiledHoistedIR of
        Just hp -> hoistedQubitCount hp
        Nothing -> 0

  let outputLayout = inferOutputLayout allFunctions

  pure
    CFGModule
      { cfgFunctions = allFunctions
      , cfgQubitCount = qubitCount
      , cfgEntryPoint = "output"
      , cfgOutputLayout = outputLayout
      }


-- | Lower an entry-point declaration (e.g. "output = expr") to a single
-- no-parameter IRFunction.
--
-- Entry-point CPS has the shape:
--   CFix [(k0, [x,...], CApp (VLabel "halt") [VVar x, ...])] outerBody
-- where outerBody is the actual computation (e.g. CApp (VLabel "bell00") [...]).
-- The wrapper functions (k0, ...) are continuation stubs; they are added to
-- fcLocalFuncs and pre-bound to "halt" in fcAtomEnv so that any reference to
-- them resolves as a continuation.  stripContArg then drops the wrapper variable
-- from the argument list of the outer call.
lowerEntryPoint
  :: String
  -> ModuleCallableKinds
  -> [(Variable, [Variable], CExp)]  -- wrapper defs from the CFix
  -> CExp                            -- outer computation body
  -> LowerM [IRFunction]
lowerEntryPoint declName callableKinds wrapperDefs outerBody = do
  let kind = case lookupTopLevelCallableKind callableKinds declName of
        Just k -> k
        Nothing -> CallableDef
      -- Pre-bind wrapper variables to "halt" so lowerVal resolves them as the
      -- continuation, and add them to fcLocalFuncs for inline resolution.
      wrapperBindings =
        Map.fromList [(v, A.AScalar (VLabel "halt")) | (v, _, _) <- wrapperDefs]
      initCtx =
        FuncCtx
          { fcTypeEnv    = Map.empty
          , fcAtomEnv    = wrapperBindings
          , fcNameEnv    = Map.empty
          , fcSelfName   = declName
          , fcLoopCtx    = Nothing
          , fcAllNames   = Map.empty
          , fcLocalFuncs = Map.fromList [(v, (ps, b)) | (v, ps, b) <- wrapperDefs]
          }
  bodyStmts <- lowerBody initCtx outerBody
  pure
    [ IRFunction
        { irFuncName   = declName
        , irFuncKind   = kind
        , irFuncParams = []
        , irFuncBody   = bodyStmts
        , irFuncIsLoop = False
        }
    ]


-- | Lower all declarations' CFix groups to IRFunctions.
lowerDecls
  :: CompiledModule
  -> [(String, HoistedProgram)]
  -> LowerM [IRFunction]
lowerDecls cm items = do
  funcsPerDecl <- forM items $ \(declName, hp) -> do
    let body = hoistedBody hp
    case body of
      -- Standard function declaration:
      --   CFix defs (CApp (VLabel "halt") [VVar exportedFn])
      -- where exportedFn is one of the defs.
      CFix defs (CApp (VLabel "halt") [VVar exportedFn])
        | exportedFn `elem` map (\(f, _, _) -> f) defs -> do
            let typeEnv = inferParamTypes defs
                nameMap = buildNameMap declName exportedFn defs
            forM defs
              (lowerFunction declName exportedFn (compiledCallableKinds cm) nameMap typeEnv)
      -- Entry-point declaration with wrappers:
      --   CFix wrappers outerBody
      -- where outerBody is the actual computation (not a halt call).
      CFix wrapperDefs outerBody ->
        lowerEntryPoint declName (compiledCallableKinds cm) wrapperDefs outerBody
      -- Bare expression (no CFix): a constant or simple expression
      -- like `hello = "hello, world"` which compiles to CApp (VLabel "halt") [VInt n].
      -- Lower directly as a no-parameter entry-point function.
      other ->
        lowerEntryPoint declName (compiledCallableKinds cm) [] other
  pure (concat funcsPerDecl)
