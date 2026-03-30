module OpenQASM
  ( emitOpenQASM
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict
import Data.List (intercalate)

import CPSExp
import BoundedRecursion (TopLevelFunction(..), extractTopLevelFunction)
import CompilePipeline
import LambdaIR (PrimOp(..))
import Utils (Variable)


data Callable
  = CallableFun [Variable] CExp Env
  | CallableTopLevel String TopLevelFunction
  | CallablePrim PrimOp [ValueRep]
  | CallableOutput


data ValueRep
  = ValueCallable Callable
  | ValueClassical ClassicalRep
  | ValueQubit Int
  | ValueCtor Int ValueRep
  | ValueRecord [ValueRep]
  | ValueSlice [ValueRep]
  | ValueUnit


data ClassicalRep
  = ClassicalIntConst Int
  | ClassicalFloatConst String
  | ClassicalVar ClassicalType String


data ClassicalType
  = CTypeInt
  | CTypeFloat
  | CTypeBool
  | CTypeBit
  deriving (Eq, Show)


data Stmt
  = StmtGate String [Int]
  | StmtMeasure String Int
  | StmtAssign String String
  | StmtDeclareAssign String String String
  | StmtIfElse String [Stmt] [Stmt]
  | StmtSwitch String [(Int, [Stmt])]


data EmitState = EmitState
  { emitNextQubit :: Int
  , emitNextBit   :: Int
  , emitNextTemp  :: Int
  , emitOutputLayout :: Maybe OutputLayout
  , emitTopLevelCache :: Map.Map String ValueRep
  , emitTopLevelInProgress :: Set.Set String
  , emitRecursiveBudget :: Int
  }

data OutputLayout
  = OutputScalars [ClassicalType]
  | OutputArray ClassicalType Int
  deriving (Eq)


type ModuleEnv = Map.Map String CExp
type Env = Map.Map Variable ValueRep
type EmitM = StateT EmitState (Either String)


emitOpenQASM :: CompiledModule -> Either String String
emitOpenQASM compiledModule = do
  rejectCompileErrors compiledModule
  outputExp <- findOutputExp compiledModule
  let moduleEnv = buildModuleEnv compiledModule
      initState =
        EmitState
          { emitNextQubit = 0
          , emitNextBit = 0
          , emitNextTemp = 0
          , emitOutputLayout = Nothing
          , emitTopLevelCache = Map.empty
          , emitTopLevelInProgress = Set.empty
          , emitRecursiveBudget = 10000
          }
  (stmts, finalState) <- runStateT (emitEntry moduleEnv outputExp) initState
  outputLayout <-
    case emitOutputLayout finalState of
      Just layout -> Right layout
      Nothing ->
        Left "OpenQASM emission failed: `output` never reached halt."
  pure $
    renderProgram
      (emitNextQubit finalState)
      outputLayout
      stmts


rejectCompileErrors :: CompiledModule -> Either String ()
rejectCompileErrors compiledModule =
  case
    [ compiledName decl ++ ": " ++ err
    | Compiled decl <- compiledItems compiledModule
    , Left err <- [compiledRecursionResult decl]
    ] of
    [] ->
      Right ()
    errs ->
      Left
        ( "OpenQASM emission failed because compilation stopped before backend emission: "
            ++ intercalate "; " errs
        )


findOutputExp :: CompiledModule -> Either String CExp
findOutputExp compiledModule =
  case
    [ interfaceExp
    | Compiled decl <- compiledItems compiledModule
    , compiledName decl == "output"
    , Just interfaceExp <- [compiledInterfaceIR decl]
    ] of
    [outputExp] -> Right outputExp
    [] ->
      Left "OpenQASM emission failed: no interface-flattened `output` declaration found."
    _ ->
      Left "OpenQASM emission failed: multiple `output` declarations found."


buildModuleEnv :: CompiledModule -> ModuleEnv
buildModuleEnv compiledModule =
  Map.fromList
    [ (compiledName decl, interfaceExp)
    | Compiled decl <- compiledItems compiledModule
    , Just interfaceExp <- [compiledInterfaceIR decl]
    ]


emitEntry :: ModuleEnv -> CExp -> EmitM [Stmt]
emitEntry moduleEnv expr =
  case expr of
    CFix ((outputK, _, _) : defs) body ->
      resultStmts <$> runExp moduleEnv entryEnv (CFix defs body) assignOutputs
      where
        entryEnv = Map.singleton outputK (ValueCallable CallableOutput)
    _ ->
      resultStmts <$> runExp moduleEnv Map.empty expr assignOutputs


assignOutputs :: [ValueRep] -> EmitM (EmitResult ())
assignOutputs values = do
  let flatValues = concatMap flattenOutputValues values
  mapM_ ensureClassicalOutput flatValues
  let outputLayout = chooseOutputLayout (map classicalTypeOfValue flatValues)
  updateOutputLayout outputLayout
  let stmts =
        [ StmtAssign (renderOutputTarget outputLayout i) (renderClassicalValue value)
        | (i, value) <- zip [0 :: Int ..] flatValues
        ]
  pure EmitResult { resultStmts = stmts, resultValue = () }


chooseOutputLayout :: [ClassicalType] -> OutputLayout
chooseOutputLayout (ty0 : ty1 : tys)
  | all (== ty0) (ty1 : tys) = OutputArray ty0 (length tys + 2)
chooseOutputLayout tys =
  OutputScalars tys


renderOutputTarget :: OutputLayout -> Int -> String
renderOutputTarget (OutputArray _ _) i = "output[" ++ show i ++ "]"
renderOutputTarget (OutputScalars _) i = "output_" ++ show i


updateOutputLayout :: OutputLayout -> EmitM ()
updateOutputLayout outputLayout = do
  current <- gets emitOutputLayout
  case current of
    Nothing ->
      modify (\st -> st { emitOutputLayout = Just outputLayout })
    Just outputLayout'
      | outputLayout' == outputLayout -> pure ()
      | otherwise ->
          lift $
            Left
              ( "OpenQASM emission failed: inconsistent output layout. Expected "
                  ++ showOutputLayout outputLayout'
                  ++ ", saw "
                  ++ showOutputLayout outputLayout
                  ++ "."
              )


data EmitResult a = EmitResult
  { resultStmts :: [Stmt]
  , resultValue :: a
  }


runExp
  :: ModuleEnv
  -> Env
  -> CExp
  -> ([ValueRep] -> EmitM (EmitResult a))
  -> EmitM (EmitResult a)
runExp moduleEnv env (CRecord fields v body) haltK = do
  fieldResults <- mapM (evalField moduleEnv env) fields
  let recordValue = buildRecordValue (map resultValue fieldResults)
  emitted <- runExp moduleEnv (Map.insert v recordValue env) body haltK
  pure emitted { resultStmts = concatMap resultStmts fieldResults ++ resultStmts emitted }

runExp moduleEnv env (CSelect i val v body) haltK = do
  valueResult <- evalValue moduleEnv env val
  selected <-
    case resultValue valueResult of
      ValueClassical (ClassicalIntConst n) ->
        lift $
          Left
            ( "OpenQASM emission failed: select "
                ++ show i
                ++ " from "
                ++ show val
                ++ " saw scalar "
                ++ show n
                ++ "."
            )
      ValueClassical (ClassicalFloatConst s) ->
        lift $
          Left
            ( "OpenQASM emission failed: select "
                ++ show i
                ++ " from "
                ++ show val
                ++ " saw float scalar "
                ++ s
                ++ "."
            )
      ValueClassical (ClassicalVar _ name) ->
        lift $
          Left
            ( "OpenQASM emission failed: select "
                ++ show i
                ++ " from scalar variable "
                ++ name
                ++ "."
            )
      selectedValue ->
        selectField i selectedValue
  emitted <- runExp moduleEnv (Map.insert v selected env) body haltK
  pure emitted { resultStmts = resultStmts valueResult ++ resultStmts emitted }

runExp moduleEnv env (COffset off val v body) haltK = do
  valueResult <- evalValue moduleEnv env val
  shifted <- offsetValue off (resultValue valueResult)
  emitted <- runExp moduleEnv (Map.insert v shifted env) body haltK
  pure emitted { resultStmts = resultStmts valueResult ++ resultStmts emitted }

runExp moduleEnv env (CApp fn args) haltK =
  case fn of
    VLabel "halt" -> do
      argResults <- mapM (evalValue moduleEnv env) args
      halted <- haltK (map resultValue argResults)
      pure halted { resultStmts = concatMap resultStmts argResults ++ resultStmts halted }
    _ -> do
      fnResult <- evalValue moduleEnv env fn
      argResults <- mapM (evalValue moduleEnv env) args
      applied <- applyCallable moduleEnv (resultValue fnResult) (map resultValue argResults) haltK
      pure
        applied
          { resultStmts =
              resultStmts fnResult ++ concatMap resultStmts argResults ++ resultStmts applied
          }

runExp moduleEnv env (CFix defs body) haltK =
  runExp moduleEnv (bindFix env defs) body haltK

runExp moduleEnv env (CSwitch val arms) haltK = do
  valueResult <- evalValue moduleEnv env val
  scrutinee <- evalSwitchScrutinee (resultValue valueResult)
  case scrutinee of
    ClassicalIntConst n
      | n >= 0 && n < length arms ->
          do
            emitted <- runExp moduleEnv env (arms !! n) haltK
            pure emitted { resultStmts = resultStmts valueResult ++ resultStmts emitted }
    ClassicalIntConst n ->
      lift $
        Left
          ( "OpenQASM emission failed: switch index "
              ++ show n
              ++ " is out of bounds."
          )
    ClassicalFloatConst symbol ->
      lift $
        Left
          ( "OpenQASM emission failed: switch scrutinee cannot be a float constant: "
              ++ symbol
              ++ "."
          )
    ClassicalVar _ name -> do
      armResults <- mapM (\arm -> runExp moduleEnv env arm haltK) arms
      case armResults of
        [] ->
          lift $
            Left "OpenQASM emission failed: switch expression has no arms."
        [falseArm, trueArm] ->
          pure $
            EmitResult
              { resultStmts =
                  resultStmts valueResult
                    ++ [StmtIfElse (name ++ " == 1") (resultStmts trueArm) (resultStmts falseArm)]
              , resultValue = resultValue trueArm
              }
        (firstArm : _) ->
          pure $
            EmitResult
              { resultStmts = resultStmts valueResult ++ [StmtSwitch name (zip [0 :: Int ..] (map resultStmts armResults))]
              , resultValue = resultValue firstArm
              }

runExp moduleEnv env (CPrimOp op args results conts) haltK =
  runPrimOp moduleEnv env op args results conts haltK


bindFix :: Env -> [(Variable, [Variable], CExp)] -> Env
bindFix env defs =
  env'
  where
    env' =
      Map.union
        (Map.fromList [ (f, ValueCallable (CallableFun params body env')) | (f, params, body) <- defs ])
        env


evalField :: ModuleEnv -> Env -> (Value, AccessPath) -> EmitM (EmitResult ValueRep)
evalField moduleEnv env (val, path) = do
  valueResult <- evalValue moduleEnv env val
  fieldValue <- applyPath path (resultValue valueResult)
  pure EmitResult
    { resultStmts = resultStmts valueResult
    , resultValue = fieldValue
    }


applyPath :: AccessPath -> ValueRep -> EmitM ValueRep
applyPath (OFFp 0) value =
  pure value
applyPath (OFFp n) (ValueCtor tag payload)
  | n <= 0 =
      pure (ValueCtor tag payload)
  | otherwise =
      applyPath (OFFp (n - 1)) (ValueRecord [payload, ValueClassical (ClassicalIntConst tag)])
applyPath (OFFp n) (ValueRecord fields) =
  pure (ValueSlice (drop n fields))
applyPath (OFFp n) (ValueSlice fields) =
  pure (ValueSlice (drop n fields))
applyPath (OFFp _) value =
  lift $
    Left
      ( "OpenQASM emission failed: cannot offset non-record value "
          ++ showValueRep value
          ++ "."
      )
applyPath (SELp i path) value =
  applyPath path =<< selectField i value


selectField :: Int -> ValueRep -> EmitM ValueRep
selectField 0 (ValueCtor _ payload) =
  pure payload
selectField 1 (ValueCtor tag _) =
  pure (ValueClassical (ClassicalIntConst tag))
selectField i (ValueRecord fields)
  | i >= 0 && i < length fields = pure (fields !! i)
selectField i (ValueSlice fields)
  | i >= 0 && i < length fields = pure (fields !! i)
selectField i value =
  lift $
    Left
      ( "OpenQASM emission failed: cannot select field "
          ++ show i
          ++ " from "
          ++ showDeepValueRep value
          ++ "."
      )


offsetValue :: Int -> ValueRep -> EmitM ValueRep
offsetValue 0 value =
  pure value
offsetValue off (ValueCtor tag payload)
  | off > 0 =
      offsetValue (off - 1) (ValueRecord [payload, ValueClassical (ClassicalIntConst tag)])
offsetValue off (ValueRecord fields) =
  pure (ValueSlice (drop off fields))
offsetValue off (ValueSlice fields) =
  pure (ValueSlice (drop off fields))
offsetValue off value =
  lift $
    Left
      ( "OpenQASM emission failed: cannot offset "
          ++ show off
          ++ " on "
          ++ showValueRep value
          ++ "."
      )


evalValue :: ModuleEnv -> Env -> Value -> EmitM (EmitResult ValueRep)
evalValue _ env (VVar v) =
  case Map.lookup v env of
    Just value -> pure EmitResult { resultStmts = [], resultValue = value }
    Nothing ->
      lift $
        Left ("OpenQASM emission failed: unbound variable " ++ show v ++ ".")
evalValue _ _ (VInt n) =
  pure EmitResult { resultStmts = [], resultValue = ValueClassical (ClassicalIntConst n) }
evalValue _ _ (VFloat s) =
  pure EmitResult { resultStmts = [], resultValue = ValueClassical (ClassicalFloatConst s) }
evalValue _ _ (VQubit i) =
  pure EmitResult { resultStmts = [], resultValue = ValueQubit i }
evalValue _ _ VUnit =
  pure EmitResult { resultStmts = [], resultValue = ValueUnit }
evalValue moduleEnv _ (VLabel name) =
  evalTopLevelValue moduleEnv name


evalTopLevelValue :: ModuleEnv -> String -> EmitM (EmitResult ValueRep)
evalTopLevelValue moduleEnv name = do
  cached <- gets (Map.lookup name . emitTopLevelCache)
  case cached of
    Just value ->
      pure EmitResult { resultStmts = [], resultValue = value }
    Nothing ->
      do
        inProgress <- gets (Set.member name . emitTopLevelInProgress)
        if inProgress
          then
            lift $
              Left
                ( "OpenQASM emission failed: recursive top-level declaration `"
                    ++ name
                    ++ "` reached the backend. Recursion elimination currently misses top-level label recursion."
                )
          else
            case Map.lookup name moduleEnv of
              Nothing ->
                lift $
                  Left ("OpenQASM emission failed: unknown top-level label `" ++ name ++ "`.")
              Just expr
                | Just topLevelFun <- extractTopLevelFunction expr -> do
                    let value = ValueCallable (CallableTopLevel name topLevelFun)
                    modify
                      (\st ->
                        st
                          { emitTopLevelCache = Map.insert name value (emitTopLevelCache st)
                          }
                      )
                    pure EmitResult { resultStmts = [], resultValue = value }
              Just expr -> do
                modify (\st -> st { emitTopLevelInProgress = Set.insert name (emitTopLevelInProgress st) })
                result <- runExp moduleEnv Map.empty expr captureValue
                modify (\st -> st { emitTopLevelInProgress = Set.delete name (emitTopLevelInProgress st) })
                case resultStmts result of
                  [] ->
                    let value = resultValue result
                    in do
                      modify (\st -> st { emitTopLevelCache = Map.insert name value (emitTopLevelCache st) })
                      pure EmitResult { resultStmts = [], resultValue = value }
                  _ ->
                    pure result
  where
    captureValue values =
      case values of
        [value] ->
          pure EmitResult { resultStmts = [], resultValue = value }
        _ ->
          lift $
            Left
              ( "OpenQASM emission failed: top-level label `"
                  ++ name
                  ++ "` returned "
                  ++ show (length values)
                  ++ " values instead of one."
              )


applyCallable
  :: ModuleEnv
  -> ValueRep
  -> [ValueRep]
  -> ([ValueRep] -> EmitM (EmitResult a))
  -> EmitM (EmitResult a)
applyCallable moduleEnv (ValueCallable (CallableFun params body closureEnv)) args haltK
  | length params == length args =
      runExp moduleEnv callEnv body haltK
  | otherwise =
      lift $
        Left
          ( "OpenQASM emission failed: arity mismatch. Expected "
              ++ show (length params)
              ++ " arguments, got "
              ++ show (length args)
              ++ "."
          )
  where
    callEnv = Map.union (Map.fromList (zip params args)) closureEnv
applyCallable moduleEnv (ValueCallable (CallableTopLevel name topLevelFun)) args haltK =
  applyTopLevelCallable moduleEnv name topLevelFun args haltK
applyCallable moduleEnv (ValueCallable (CallablePrim op captured)) args haltK =
  applyPrimitiveCallable moduleEnv op captured args haltK
applyCallable _ (ValueCallable CallableOutput) args haltK =
  haltK args
applyCallable _ value _ _ =
  lift $
    Left
      ( "OpenQASM emission failed: attempted to call non-callable value "
          ++ showValueRep value
          ++ "."
      )


applyTopLevelCallable
  :: ModuleEnv
  -> String
  -> TopLevelFunction
  -> [ValueRep]
  -> ([ValueRep] -> EmitM (EmitResult a))
  -> EmitM (EmitResult a)
applyTopLevelCallable moduleEnv name topLevelFun args haltK
  | length (tlfParams topLevelFun) /= length args =
      lift $
        Left
          ( "OpenQASM emission failed: arity mismatch for top-level declaration `"
              ++ name
              ++ "`. Expected "
              ++ show (length (tlfParams topLevelFun))
              ++ " arguments, got "
              ++ show (length args)
              ++ "."
          )
  | otherwise =
      do
        ensureRecursiveBudget name
        runExp moduleEnv (Map.fromList (zip (tlfParams topLevelFun) args)) (tlfBody topLevelFun) haltK


ensureRecursiveBudget :: String -> EmitM ()
ensureRecursiveBudget name = do
  remaining <- gets emitRecursiveBudget
  if remaining <= 0
    then
      lift $
        Left
          ( "OpenQASM emission failed: bounded recursion budget exhausted while expanding `"
              ++ name
              ++ "`. Refusing to continue unrolling."
          )
    else
      modify (\st -> st { emitRecursiveBudget = remaining - 1 })


applyPrimitiveCallable
  :: ModuleEnv
  -> PrimOp
  -> [ValueRep]
  -> [ValueRep]
  -> ([ValueRep] -> EmitM (EmitResult a))
  -> EmitM (EmitResult a)
applyPrimitiveCallable moduleEnv op captured args haltK =
  case reverse args of
    [] ->
      lift $
        Left
          ( "OpenQASM emission failed: primitive "
              ++ show op
              ++ " was called without a continuation."
          )
    (contVal : primArgsRev) -> do
      let primArgs = captured ++ reverse primArgsRev
      if length primArgs < primArity op
        then do
          applied <- applyCallable moduleEnv contVal [ValueCallable (CallablePrim op primArgs)] haltK
          pure applied
        else if length primArgs == primArity op
          then runPrimitiveExecution moduleEnv op primArgs contVal haltK
          else
            lift $
              Left
                ( "OpenQASM emission failed: primitive "
                    ++ show op
                    ++ " received too many arguments."
                )


runPrimOp
  :: ModuleEnv
  -> Env
  -> PrimOp
  -> [Value]
  -> [Variable]
  -> [CExp]
  -> ([ValueRep] -> EmitM (EmitResult a))
  -> EmitM (EmitResult a)
runPrimOp moduleEnv env op args results conts haltK = do
  argResults <- mapM (evalValue moduleEnv env) args
  let argVals = map resultValue argResults
      argStmts = concatMap resultStmts argResults
  if length argVals < primArity op
    then case (results, conts) of
      ([result], [cont]) -> do
        let env' = Map.insert result (ValueCallable (CallablePrim op argVals)) env
        emitted <- runExp moduleEnv env' cont haltK
        pure emitted { resultStmts = argStmts ++ resultStmts emitted }
      _ ->
        lift $
          Left
            ( "OpenQASM emission failed: under-applied primitive "
                ++ show op
                ++ " must bind one function value."
            )
    else if length argVals == primArity op
      then do
        emitted <- runDirectPrimOp moduleEnv env op argVals results conts haltK
        pure emitted { resultStmts = argStmts ++ resultStmts emitted }
      else
        lift $
          Left
            ( "OpenQASM emission failed: primitive "
                ++ show op
                ++ " received too many direct arguments."
            )


runDirectPrimOp
  :: ModuleEnv
  -> Env
  -> PrimOp
  -> [ValueRep]
  -> [Variable]
  -> [CExp]
  -> ([ValueRep] -> EmitM (EmitResult a))
  -> EmitM (EmitResult a)
runDirectPrimOp moduleEnv env PInit [ValueUnit] [q] [cont] haltK = do
  slot <- freshQubit
  runExp moduleEnv (Map.insert q (ValueQubit slot) env) cont haltK
runDirectPrimOp moduleEnv env PHGate [arg] [result] [cont] haltK
  | Just q <- extractQubitArg arg = do
  emitted <- runExp moduleEnv (Map.insert result (ValueQubit q) env) cont haltK
  pure emitted { resultStmts = StmtGate "h" [q] : resultStmts emitted }
runDirectPrimOp moduleEnv env PXGate [arg] [result] [cont] haltK
  | Just q <- extractQubitArg arg = do
  emitted <- runExp moduleEnv (Map.insert result (ValueQubit q) env) cont haltK
  pure emitted { resultStmts = StmtGate "x" [q] : resultStmts emitted }
runDirectPrimOp moduleEnv env PZGate [arg] [result] [cont] haltK
  | Just q <- extractQubitArg arg = do
  emitted <- runExp moduleEnv (Map.insert result (ValueQubit q) env) cont haltK
  pure emitted { resultStmts = StmtGate "z" [q] : resultStmts emitted }
runDirectPrimOp moduleEnv env PCNot [arg0, arg1] [r0, r1] [cont] haltK
  | Just q0 <- extractQubitArg arg0
  , Just q1 <- extractQubitArg arg1 = do
  let env' =
        Map.insert r1 (ValueQubit q1) (Map.insert r0 (ValueQubit q0) env)
  emitted <- runExp moduleEnv env' cont haltK
  pure emitted { resultStmts = StmtGate "cx" [q0, q1] : resultStmts emitted }
runDirectPrimOp moduleEnv env PSGate [arg] [result] [cont] haltK
  | Just q <- extractQubitArg arg = do
  emitted <- runExp moduleEnv (Map.insert result (ValueQubit q) env) cont haltK
  pure emitted { resultStmts = StmtGate "s" [q] : resultStmts emitted }
runDirectPrimOp moduleEnv env PTGate [arg] [result] [cont] haltK
  | Just q <- extractQubitArg arg = do
  emitted <- runExp moduleEnv (Map.insert result (ValueQubit q) env) cont haltK
  pure emitted { resultStmts = StmtGate "t" [q] : resultStmts emitted }
runDirectPrimOp moduleEnv env PCSGate [arg0, arg1] [r0, r1] [cont] haltK
  | Just q0 <- extractQubitArg arg0
  , Just q1 <- extractQubitArg arg1 = do
  let env' =
        Map.insert r1 (ValueQubit q1) (Map.insert r0 (ValueQubit q0) env)
  emitted <- runExp moduleEnv env' cont haltK
  pure emitted { resultStmts = StmtGate "cp(pi/2)" [q0, q1] : resultStmts emitted }
runDirectPrimOp moduleEnv env PCTGate [arg0, arg1] [r0, r1] [cont] haltK
  | Just q0 <- extractQubitArg arg0
  , Just q1 <- extractQubitArg arg1 = do
  let env' =
        Map.insert r1 (ValueQubit q1) (Map.insert r0 (ValueQubit q0) env)
  emitted <- runExp moduleEnv env' cont haltK
  pure emitted { resultStmts = StmtGate "cp(pi/4)" [q0, q1] : resultStmts emitted }
runDirectPrimOp moduleEnv env PCpGate [ValueClassical (ClassicalIntConst k), arg0, arg1] [r0, r1] [cont] haltK
  | Just q0 <- extractQubitArg arg0
  , Just q1 <- extractQubitArg arg1 = do
  let env' =
        Map.insert r1 (ValueQubit q1) (Map.insert r0 (ValueQubit q0) env)
      gateName = "cp(pi/" ++ show (2 ^ k :: Int) ++ ")"
  emitted <- runExp moduleEnv env' cont haltK
  pure emitted { resultStmts = StmtGate gateName [q0, q1] : resultStmts emitted }
runDirectPrimOp moduleEnv env PMeas [arg] [result] [cont] haltK
  | Just q <- extractQubitArg arg = do
  bitName <- freshBitName
  let env' = Map.insert result (ValueClassical (ClassicalVar CTypeBit bitName)) env
  emitted <- runExp moduleEnv env' cont haltK
  pure emitted { resultStmts = StmtMeasure bitName q : resultStmts emitted }
runDirectPrimOp moduleEnv env op args [result] [cont] haltK
  | isClassicalPrim op = do
      case evalClassicalPrimConst op args of
        Just constValue -> do
          let env' = Map.insert result (ValueClassical (ClassicalIntConst constValue)) env
          runExp moduleEnv env' cont haltK
        Nothing -> do
          tempName <- freshTempName
          let resultType = inferClassicalPrimType op args
              env' = Map.insert result (ValueClassical (ClassicalVar resultType tempName)) env
              declType = renderClassicalType resultType
          emitted <- runExp moduleEnv env' cont haltK
          pure
            emitted
              { resultStmts =
                  StmtDeclareAssign declType tempName (renderClassicalPrim op args)
                    : resultStmts emitted
              }
runDirectPrimOp _ _ op args results conts _ =
  lift $
    Left
      ( "OpenQASM emission failed: unsupported primitive form "
          ++ show op
          ++ " with "
          ++ show (length args)
          ++ " args, "
          ++ show (length results)
          ++ " results, and "
          ++ show (length conts)
          ++ " continuations. Args were ["
          ++ intercalate ", " (map showDeepValueRep args)
          ++ "]."
      )


runPrimitiveExecution
  :: ModuleEnv
  -> PrimOp
  -> [ValueRep]
  -> ValueRep
  -> ([ValueRep] -> EmitM (EmitResult a))
  -> EmitM (EmitResult a)
runPrimitiveExecution moduleEnv PHGate [arg] cont haltK
  | Just q <- extractQubitArg arg = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q] haltK
  pure emitted { resultStmts = StmtGate "h" [q] : resultStmts emitted }
runPrimitiveExecution moduleEnv PXGate [arg] cont haltK
  | Just q <- extractQubitArg arg = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q] haltK
  pure emitted { resultStmts = StmtGate "x" [q] : resultStmts emitted }
runPrimitiveExecution moduleEnv PZGate [arg] cont haltK
  | Just q <- extractQubitArg arg = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q] haltK
  pure emitted { resultStmts = StmtGate "z" [q] : resultStmts emitted }
runPrimitiveExecution moduleEnv PCNot [arg0, arg1] cont haltK
  | Just q0 <- extractQubitArg arg0
  , Just q1 <- extractQubitArg arg1 = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q0, ValueQubit q1] haltK
  pure emitted { resultStmts = StmtGate "cx" [q0, q1] : resultStmts emitted }
runPrimitiveExecution moduleEnv PSGate [arg] cont haltK
  | Just q <- extractQubitArg arg = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q] haltK
  pure emitted { resultStmts = StmtGate "s" [q] : resultStmts emitted }
runPrimitiveExecution moduleEnv PTGate [arg] cont haltK
  | Just q <- extractQubitArg arg = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q] haltK
  pure emitted { resultStmts = StmtGate "t" [q] : resultStmts emitted }
runPrimitiveExecution moduleEnv PCSGate [arg0, arg1] cont haltK
  | Just q0 <- extractQubitArg arg0
  , Just q1 <- extractQubitArg arg1 = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q0, ValueQubit q1] haltK
  pure emitted { resultStmts = StmtGate "cp(pi/2)" [q0, q1] : resultStmts emitted }
runPrimitiveExecution moduleEnv PCTGate [arg0, arg1] cont haltK
  | Just q0 <- extractQubitArg arg0
  , Just q1 <- extractQubitArg arg1 = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q0, ValueQubit q1] haltK
  pure emitted { resultStmts = StmtGate "cp(pi/4)" [q0, q1] : resultStmts emitted }
runPrimitiveExecution moduleEnv PCpGate [ValueClassical (ClassicalIntConst k), arg0, arg1] cont haltK
  | Just q0 <- extractQubitArg arg0
  , Just q1 <- extractQubitArg arg1 = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q0, ValueQubit q1] haltK
  let gateName = "cp(pi/" ++ show (2 ^ k :: Int) ++ ")"
  pure emitted { resultStmts = StmtGate gateName [q0, q1] : resultStmts emitted }
runPrimitiveExecution moduleEnv PMeas [arg] cont haltK
  | Just q <- extractQubitArg arg = do
  bitName <- freshBitName
  emitted <- applyCallable moduleEnv cont [ValueClassical (ClassicalVar CTypeBit bitName)] haltK
  pure emitted { resultStmts = StmtMeasure bitName q : resultStmts emitted }
runPrimitiveExecution _ op _ _ _ =
  lift $
    Left
      ( "OpenQASM emission failed: unsupported primitive callable execution for "
          ++ show op
          ++ "."
      )


primArity :: PrimOp -> Int
primArity PInit = 1
primArity PMeas = 1
primArity PHGate = 1
primArity PXGate = 1
primArity PZGate = 1
primArity PCNot   = 2
primArity PSGate  = 1
primArity PTGate  = 1
primArity PCSGate = 2
primArity PCTGate = 2
primArity PCpGate = 3
primArity PAdd = 2
primArity PSub = 2
primArity PMul = 2
primArity PDiv = 2
primArity PEq = 2
primArity PLt = 2
primArity PGt = 2
primArity PLe = 2
primArity PGe = 2
primArity PAnd = 2
primArity POr = 2
primArity PNot = 1


isClassicalPrim :: PrimOp -> Bool
isClassicalPrim op =
  op `elem` [PAdd, PSub, PMul, PDiv, PEq, PLt, PGt, PLe, PGe, PAnd, POr, PNot]


isBooleanPrim :: PrimOp -> Bool
isBooleanPrim op =
  op `elem` [PEq, PLt, PGt, PLe, PGe, PAnd, POr, PNot]


renderClassicalPrim :: PrimOp -> [ValueRep] -> String
renderClassicalPrim PAdd [a, b] = renderClassicalValue a ++ " + " ++ renderClassicalValue b
renderClassicalPrim PSub [a, b] = renderClassicalValue a ++ " - " ++ renderClassicalValue b
renderClassicalPrim PMul [a, b] = renderClassicalValue a ++ " * " ++ renderClassicalValue b
renderClassicalPrim PDiv [a, b] = renderClassicalValue a ++ " / " ++ renderClassicalValue b
renderClassicalPrim PEq [a, b] = renderClassicalValue a ++ " == " ++ renderClassicalValue b
renderClassicalPrim PLt [a, b] = renderClassicalValue a ++ " < " ++ renderClassicalValue b
renderClassicalPrim PGt [a, b] = renderClassicalValue a ++ " > " ++ renderClassicalValue b
renderClassicalPrim PLe [a, b] = renderClassicalValue a ++ " <= " ++ renderClassicalValue b
renderClassicalPrim PGe [a, b] = renderClassicalValue a ++ " >= " ++ renderClassicalValue b
renderClassicalPrim PAnd [a, b] = renderClassicalValue a ++ " && " ++ renderClassicalValue b
renderClassicalPrim POr [a, b] = renderClassicalValue a ++ " || " ++ renderClassicalValue b
renderClassicalPrim PNot [a] = "!" ++ renderClassicalValue a
renderClassicalPrim op args =
  error ("unhandled classical primitive rendering: " ++ show op ++ " " ++ show (length args))


evalClassicalPrimConst :: PrimOp -> [ValueRep] -> Maybe Int
evalClassicalPrimConst op args =
  mapM asConst args >>= eval op
  where
    asConst (ValueClassical (ClassicalIntConst n)) = Just n
    asConst _ = Nothing

    boolToInt False = 0
    boolToInt True = 1

    eval PAdd [a, b] = Just (a + b)
    eval PSub [a, b] = Just (a - b)
    eval PMul [a, b] = Just (a * b)
    eval PDiv [a, b]
      | b == 0 = Nothing
      | otherwise = Just (a `div` b)
    eval PEq [a, b] = Just (boolToInt (a == b))
    eval PLt [a, b] = Just (boolToInt (a < b))
    eval PGt [a, b] = Just (boolToInt (a > b))
    eval PLe [a, b] = Just (boolToInt (a <= b))
    eval PGe [a, b] = Just (boolToInt (a >= b))
    eval PAnd [a, b] = Just (boolToInt (a /= 0 && b /= 0))
    eval POr [a, b] = Just (boolToInt (a /= 0 || b /= 0))
    eval PNot [a] = Just (boolToInt (a == 0))
    eval _ _ = Nothing


extractQubitArg :: ValueRep -> Maybe Int
extractQubitArg (ValueQubit q) =
  Just q
extractQubitArg (ValueRecord (headValue : _)) =
  extractQubitArg headValue
extractQubitArg (ValueSlice (headValue : _)) =
  extractQubitArg headValue
extractQubitArg (ValueCtor _ payload) =
  extractQubitArg payload
extractQubitArg _ =
  Nothing


evalClassicalValue :: ValueRep -> EmitM ClassicalRep
evalClassicalValue (ValueClassical value) =
  pure value
evalClassicalValue value =
  lift $
    Left
      ( "OpenQASM emission failed: expected classical value, got "
          ++ showValueRep value
          ++ "."
      )


evalSwitchScrutinee :: ValueRep -> EmitM ClassicalRep
evalSwitchScrutinee (ValueCtor tag _) =
  pure (ClassicalIntConst tag)
evalSwitchScrutinee (ValueRecord [_, ValueClassical tag]) =
  pure tag
evalSwitchScrutinee (ValueSlice (_ : ValueClassical tag : _)) =
  pure tag
evalSwitchScrutinee value =
  evalClassicalValue value


renderClassicalValue :: ValueRep -> String
renderClassicalValue (ValueClassical (ClassicalIntConst n)) = show n
renderClassicalValue (ValueClassical (ClassicalFloatConst s)) = s
renderClassicalValue (ValueClassical (ClassicalVar _ name)) = name
renderClassicalValue value =
  error ("attempted to render non-classical value: " ++ showValueRep value)


renderClassicalType :: ClassicalType -> String
renderClassicalType CTypeInt = "int[32]"
renderClassicalType CTypeFloat = "float[64]"
renderClassicalType CTypeBool = "bool"
renderClassicalType CTypeBit = "bit"


classicalTypeOfValue :: ValueRep -> ClassicalType
classicalTypeOfValue (ValueClassical (ClassicalFloatConst _)) = CTypeFloat
classicalTypeOfValue (ValueClassical (ClassicalVar ty _)) = ty
classicalTypeOfValue _ = CTypeInt


isFloatClassicalValue :: ValueRep -> Bool
isFloatClassicalValue (ValueClassical (ClassicalFloatConst _)) = True
isFloatClassicalValue (ValueClassical (ClassicalVar CTypeFloat _)) = True
isFloatClassicalValue _ = False


inferClassicalPrimType :: PrimOp -> [ValueRep] -> ClassicalType
inferClassicalPrimType op args
  | isBooleanPrim op = CTypeBool
  | any isFloatClassicalValue args = CTypeFloat
  | otherwise = CTypeInt


freshQubit :: EmitM Int
freshQubit = do
  nextQubit <- gets emitNextQubit
  modify (\st -> st { emitNextQubit = nextQubit + 1 })
  pure nextQubit


freshBitName :: EmitM String
freshBitName = do
  nextBit <- gets emitNextBit
  modify (\st -> st { emitNextBit = nextBit + 1 })
  pure ("m" ++ show nextBit)


freshTempName :: EmitM String
freshTempName = do
  nextTemp <- gets emitNextTemp
  modify (\st -> st { emitNextTemp = nextTemp + 1 })
  pure ("t" ++ show nextTemp)


showValueRep :: ValueRep -> String
showValueRep (ValueCallable _) = "<callable>"
showValueRep (ValueClassical (ClassicalIntConst n)) = show n
showValueRep (ValueClassical (ClassicalFloatConst s)) = s
showValueRep (ValueClassical (ClassicalVar _ name)) = name
showValueRep (ValueQubit i) = "q[" ++ show i ++ "]"
showValueRep (ValueCtor tag _) = "<ctor:" ++ show tag ++ ">"
showValueRep (ValueRecord fields) = "<record:" ++ show (length fields) ++ ">"
showValueRep (ValueSlice fields) = "<slice:" ++ show (length fields) ++ ">"
showValueRep ValueUnit = "()"


showDeepValueRep :: ValueRep -> String
showDeepValueRep (ValueRecord fields) =
  "{" ++ intercalate ", " (map showValueRep fields) ++ "}"
showDeepValueRep (ValueCtor tag payload) =
  "<ctor " ++ show tag ++ " " ++ showDeepValueRep payload ++ ">"
showDeepValueRep (ValueSlice fields) =
  "<slice {" ++ intercalate ", " (map showValueRep fields) ++ "}>"
showDeepValueRep value =
  showValueRep value


flattenOutputValues :: ValueRep -> [ValueRep]
flattenOutputValues value =
  case flattenListValue value of
    Just elements ->
      concatMap flattenOutputValues elements
    Nothing ->
      flattenRecordLeaves value


flattenListValue :: ValueRep -> Maybe [ValueRep]
flattenListValue (ValueClassical (ClassicalIntConst 0)) =
  Just []
flattenListValue (ValueCtor 1 payload) = do
  (headValue, tailValue) <- splitConsPayload payload
  tailValues <- flattenListValue tailValue
  Just (headValue : tailValues)
flattenListValue (ValueRecord [payload, ValueClassical (ClassicalIntConst 1)]) = do
  (headValue, tailValue) <- splitConsPayload payload
  tailValues <- flattenListValue tailValue
  Just (headValue : tailValues)
flattenListValue _ =
  Nothing


splitConsPayload :: ValueRep -> Maybe (ValueRep, ValueRep)
splitConsPayload (ValueRecord [headValue, tailValue]) =
  Just (headValue, tailValue)
splitConsPayload (ValueSlice (headValue : tailValue : _)) =
  Just (headValue, tailValue)
splitConsPayload _ =
  Nothing


flattenRecordLeaves :: ValueRep -> [ValueRep]
flattenRecordLeaves (ValueRecord fields) =
  concatMap flattenRecordLeaves fields
flattenRecordLeaves (ValueCtor _ payload) =
  flattenRecordLeaves payload
flattenRecordLeaves (ValueSlice fields) =
  concatMap flattenRecordLeaves fields
flattenRecordLeaves ValueUnit =
  []
flattenRecordLeaves leaf =
  [leaf]


ensureClassicalOutput :: ValueRep -> EmitM ()
ensureClassicalOutput (ValueClassical _) =
  pure ()
ensureClassicalOutput value =
  lift $
    Left
      ( "OpenQASM emission failed: output flattening produced non-classical leaf "
          ++ showValueRep value
          ++ "."
      )


buildRecordValue :: [ValueRep] -> ValueRep
buildRecordValue [payload, ValueClassical (ClassicalIntConst tag)]
  | isCtorPayload payload =
  ValueCtor tag payload
buildRecordValue fields =
  ValueRecord fields


isCtorPayload :: ValueRep -> Bool
isCtorPayload (ValueRecord _) = True
isCtorPayload (ValueSlice _) = True
isCtorPayload ValueUnit = True
isCtorPayload _ = False


showOutputLayout :: OutputLayout -> String
showOutputLayout (OutputScalars tys) =
  "scalars " ++ show tys
showOutputLayout (OutputArray ty n) =
  "array " ++ show ty ++ "[" ++ show n ++ "]"


renderProgram :: Int -> OutputLayout -> [Stmt] -> String
renderProgram qubitCount outputLayout stmts =
  unlines $
    [ "OPENQASM 3.0;"
    , "include \"stdgates.inc\";"
    ]
      ++ qubitDecls
      ++ renderOutputDecls outputLayout
      ++ [""]
      ++ concatMap (renderStmt 0) stmts
  where
    qubitDecls
      | qubitCount <= 0 = []
      | otherwise = ["qubit[" ++ show qubitCount ++ "] q;"]


renderOutputDecls :: OutputLayout -> [String]
renderOutputDecls (OutputScalars outputTypes) =
  [ renderClassicalType ty ++ " output_" ++ show i ++ ";"
  | (i, ty) <- zip [0 :: Int ..] outputTypes
  ]
renderOutputDecls (OutputArray CTypeBit n) =
  ["bit[" ++ show n ++ "] output;"]
renderOutputDecls (OutputArray ty n) =
  ["array[" ++ renderClassicalType ty ++ ", " ++ show n ++ "] output;"]


renderStmt :: Int -> Stmt -> [String]
renderStmt indentLevel (StmtGate gateName [q]) =
  [indent indentLevel ++ gateName ++ " q[" ++ show q ++ "];"]
renderStmt indentLevel (StmtGate gateName [q0, q1]) =
  [indent indentLevel ++ gateName ++ " q[" ++ show q0 ++ "], q[" ++ show q1 ++ "];"]
renderStmt indentLevel (StmtMeasure bitName q) =
  [indent indentLevel ++ "bit " ++ bitName ++ " = measure q[" ++ show q ++ "];"]
renderStmt indentLevel (StmtAssign name valueExpr) =
  [indent indentLevel ++ name ++ " = " ++ valueExpr ++ ";"]
renderStmt indentLevel (StmtDeclareAssign declType name valueExpr) =
  [indent indentLevel ++ declType ++ " " ++ name ++ " = " ++ valueExpr ++ ";"]
renderStmt indentLevel (StmtIfElse condition trueStmts falseStmts) =
  [indent indentLevel ++ "if (" ++ condition ++ ") {"]
    ++ concatMap (renderStmt (indentLevel + 1)) trueStmts
    ++ [indent indentLevel ++ "} else {"]
    ++ concatMap (renderStmt (indentLevel + 1)) falseStmts
    ++ [indent indentLevel ++ "}"]
renderStmt indentLevel (StmtSwitch scrutinee arms) =
  [indent indentLevel ++ "switch (" ++ scrutinee ++ ") {"]
    ++ concatMap renderArm arms
    ++ [indent indentLevel ++ "}"]
  where
    renderArm (tag, armStmts) =
      [indent (indentLevel + 1) ++ "case " ++ show tag ++ ":"]
        ++ [indent (indentLevel + 2) ++ "{"]
        ++ concatMap (renderStmt (indentLevel + 3)) armStmts
        ++ [indent (indentLevel + 2) ++ "}"]
renderStmt _ (StmtGate _ qs) =
  error ("unsupported gate statement arity: " ++ show (length qs))


indent :: Int -> String
indent n =
  replicate (2 * n) ' '
