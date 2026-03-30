module OpenQASM
  ( emitOpenQASM
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict
import Data.List (intercalate)

import CPSExp
import CompilePipeline
import LambdaIR (PrimOp(..))
import Utils (Variable)


data Callable
  = CallableFun [Variable] CExp Env
  | CallablePrim PrimOp [ValueRep]
  | CallableOutput


data ValueRep
  = ValueCallable Callable
  | ValueClassical ClassicalRep
  | ValueQubit Int
  | ValueRecord [ValueRep]
  | ValueSlice [ValueRep]
  | ValueUnit


data ClassicalRep
  = ClassicalConst Int
  | ClassicalVar String


data Stmt
  = StmtGate String [Int]
  | StmtMeasure String Int [Stmt] [Stmt]
  | StmtAssign String String
  | StmtDeclareAssign String String String
  | StmtSwitch String [(Int, [Stmt])]


data EmitState = EmitState
  { emitNextQubit :: Int
  , emitNextBit   :: Int
  , emitNextTemp  :: Int
  , emitOutputArity :: Maybe Int
  , emitTopLevelCache :: Map.Map String ValueRep
  , emitTopLevelInProgress :: Set.Set String
  }


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
          , emitOutputArity = Nothing
          , emitTopLevelCache = Map.empty
          , emitTopLevelInProgress = Set.empty
          }
  (stmts, finalState) <- runStateT (emitEntry moduleEnv outputExp) initState
  outputArity <-
    case emitOutputArity finalState of
      Just arity -> Right arity
      Nothing ->
        Left "OpenQASM emission failed: `output` never reached halt."
  pure $
    renderProgram
      (emitNextQubit finalState)
      outputArity
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
  updateOutputArity (length values)
  let stmts =
        [ StmtAssign ("output_" ++ show i) (renderClassicalValue value)
        | (i, value) <- zip [0 :: Int ..] values
        ]
  pure EmitResult { resultStmts = stmts, resultValue = () }


updateOutputArity :: Int -> EmitM ()
updateOutputArity arity = do
  current <- gets emitOutputArity
  case current of
    Nothing ->
      modify (\st -> st { emitOutputArity = Just arity })
    Just arity'
      | arity' == arity -> pure ()
      | otherwise ->
          lift $
            Left
              ( "OpenQASM emission failed: inconsistent output arity. Expected "
                  ++ show arity'
                  ++ ", saw "
                  ++ show arity
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
  fieldVals <- mapM (evalField moduleEnv env) fields
  runExp moduleEnv (Map.insert v (ValueRecord fieldVals) env) body haltK

runExp moduleEnv env (CSelect i val v body) haltK = do
  selected <- selectField i =<< evalValue moduleEnv env val
  runExp moduleEnv (Map.insert v selected env) body haltK

runExp moduleEnv env (COffset off val v body) haltK = do
  shifted <- offsetValue off =<< evalValue moduleEnv env val
  runExp moduleEnv (Map.insert v shifted env) body haltK

runExp moduleEnv env (CApp fn args) haltK =
  case fn of
    VLabel "halt" -> do
      argVals <- mapM (evalValue moduleEnv env) args
      haltK argVals
    _ -> do
      fnVal <- evalValue moduleEnv env fn
      argVals <- mapM (evalValue moduleEnv env) args
      applyCallable moduleEnv fnVal argVals haltK

runExp moduleEnv env (CFix defs body) haltK =
  runExp moduleEnv (bindFix env defs) body haltK

runExp moduleEnv env (CSwitch val arms) haltK = do
  scrutinee <- evalClassicalValue =<< evalValue moduleEnv env val
  case scrutinee of
    ClassicalConst n
      | n >= 0 && n < length arms ->
          runExp moduleEnv env (arms !! n) haltK
    ClassicalConst n ->
      lift $
        Left
          ( "OpenQASM emission failed: switch index "
              ++ show n
              ++ " is out of bounds."
          )
    ClassicalVar name -> do
      armResults <- mapM (\arm -> runExp moduleEnv env arm haltK) arms
      case armResults of
        [] ->
          lift $
            Left "OpenQASM emission failed: switch expression has no arms."
        (firstArm : _) ->
          pure $
            EmitResult
              { resultStmts = [StmtSwitch name (zip [0 :: Int ..] (map resultStmts armResults))]
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


evalField :: ModuleEnv -> Env -> (Value, AccessPath) -> EmitM ValueRep
evalField moduleEnv env (val, path) =
  applyPath path =<< evalValue moduleEnv env val


applyPath :: AccessPath -> ValueRep -> EmitM ValueRep
applyPath (OFFp 0) value =
  pure value
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
          ++ showValueRep value
          ++ "."
      )


offsetValue :: Int -> ValueRep -> EmitM ValueRep
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


evalValue :: ModuleEnv -> Env -> Value -> EmitM ValueRep
evalValue _ env (VVar v) =
  case Map.lookup v env of
    Just value -> pure value
    Nothing ->
      lift $
        Left ("OpenQASM emission failed: unbound variable " ++ show v ++ ".")
evalValue _ _ (VInt n) =
  pure (ValueClassical (ClassicalConst n))
evalValue _ _ (VQubit i) =
  pure (ValueQubit i)
evalValue _ _ VUnit =
  pure ValueUnit
evalValue moduleEnv _ (VLabel name) =
  evalTopLevelValue moduleEnv name


evalTopLevelValue :: ModuleEnv -> String -> EmitM ValueRep
evalTopLevelValue moduleEnv name = do
  cached <- gets (Map.lookup name . emitTopLevelCache)
  case cached of
    Just value ->
      pure value
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
              Just expr -> do
                modify (\st -> st { emitTopLevelInProgress = Set.insert name (emitTopLevelInProgress st) })
                result <- runExp moduleEnv Map.empty expr captureValue
                modify (\st -> st { emitTopLevelInProgress = Set.delete name (emitTopLevelInProgress st) })
                case resultStmts result of
                  [] ->
                    let value = resultValue result
                    in do
                      modify (\st -> st { emitTopLevelCache = Map.insert name value (emitTopLevelCache st) })
                      pure value
                  _ ->
                    lift $
                      Left
                        ( "OpenQASM emission failed: top-level label `"
                            ++ name
                            ++ "` produced statements while being evaluated as a value."
                        )
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
  argVals <- mapM (evalValue moduleEnv env) args
  if length argVals < primArity op
    then case (results, conts) of
      ([result], [cont]) -> do
        let env' = Map.insert result (ValueCallable (CallablePrim op argVals)) env
        runExp moduleEnv env' cont haltK
      _ ->
        lift $
          Left
            ( "OpenQASM emission failed: under-applied primitive "
                ++ show op
                ++ " must bind one function value."
            )
    else if length argVals == primArity op
      then runDirectPrimOp moduleEnv env op argVals results conts haltK
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
runDirectPrimOp moduleEnv env PHGate [ValueQubit q] [result] [cont] haltK = do
  emitted <- runExp moduleEnv (Map.insert result (ValueQubit q) env) cont haltK
  pure emitted { resultStmts = StmtGate "h" [q] : resultStmts emitted }
runDirectPrimOp moduleEnv env PXGate [ValueQubit q] [result] [cont] haltK = do
  emitted <- runExp moduleEnv (Map.insert result (ValueQubit q) env) cont haltK
  pure emitted { resultStmts = StmtGate "x" [q] : resultStmts emitted }
runDirectPrimOp moduleEnv env PZGate [ValueQubit q] [result] [cont] haltK = do
  emitted <- runExp moduleEnv (Map.insert result (ValueQubit q) env) cont haltK
  pure emitted { resultStmts = StmtGate "z" [q] : resultStmts emitted }
runDirectPrimOp moduleEnv env PCNot [ValueQubit q0, ValueQubit q1] [r0, r1] [cont] haltK = do
  emitted <- runExp moduleEnv env' cont haltK
  pure emitted { resultStmts = StmtGate "cx" [q0, q1] : resultStmts emitted }
  where
    env' =
      Map.insert r1 (ValueQubit q1) (Map.insert r0 (ValueQubit q0) env)
runDirectPrimOp moduleEnv env PMeas [ValueQubit q] [] [zeroCont, oneCont] haltK = do
  bitName <- freshBitName
  zeroResult <- runExp moduleEnv env zeroCont haltK
  oneResult <- runExp moduleEnv env oneCont haltK
  pure
    EmitResult
      { resultStmts =
          [StmtMeasure bitName q (resultStmts zeroResult) (resultStmts oneResult)]
      , resultValue = resultValue zeroResult
      }
runDirectPrimOp moduleEnv env op args [result] [cont] haltK
  | isClassicalPrim op = do
      tempName <- freshTempName
      let env' = Map.insert result (ValueClassical (ClassicalVar tempName)) env
          declType = if isBooleanPrim op then "bool" else "int[32]"
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
          ++ " continuations."
      )


runPrimitiveExecution
  :: ModuleEnv
  -> PrimOp
  -> [ValueRep]
  -> ValueRep
  -> ([ValueRep] -> EmitM (EmitResult a))
  -> EmitM (EmitResult a)
runPrimitiveExecution moduleEnv PHGate [ValueQubit q] cont haltK = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q] haltK
  pure emitted { resultStmts = StmtGate "h" [q] : resultStmts emitted }
runPrimitiveExecution moduleEnv PXGate [ValueQubit q] cont haltK = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q] haltK
  pure emitted { resultStmts = StmtGate "x" [q] : resultStmts emitted }
runPrimitiveExecution moduleEnv PZGate [ValueQubit q] cont haltK = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q] haltK
  pure emitted { resultStmts = StmtGate "z" [q] : resultStmts emitted }
runPrimitiveExecution moduleEnv PCNot [ValueQubit q0, ValueQubit q1] cont haltK = do
  emitted <- applyCallable moduleEnv cont [ValueQubit q0, ValueQubit q1] haltK
  pure emitted { resultStmts = StmtGate "cx" [q0, q1] : resultStmts emitted }
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
primArity PCNot = 2
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


renderClassicalValue :: ValueRep -> String
renderClassicalValue (ValueClassical (ClassicalConst n)) = show n
renderClassicalValue (ValueClassical (ClassicalVar name)) = name
renderClassicalValue value =
  error ("attempted to render non-classical value: " ++ showValueRep value)


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
showValueRep (ValueClassical (ClassicalConst n)) = show n
showValueRep (ValueClassical (ClassicalVar name)) = name
showValueRep (ValueQubit i) = "q[" ++ show i ++ "]"
showValueRep (ValueRecord fields) = "<record:" ++ show (length fields) ++ ">"
showValueRep (ValueSlice fields) = "<slice:" ++ show (length fields) ++ ">"
showValueRep ValueUnit = "()"


renderProgram :: Int -> Int -> [Stmt] -> String
renderProgram qubitCount outputArity stmts =
  unlines $
    [ "OPENQASM 3.0;"
    , "include \"stdgates.inc\";"
    ]
      ++ qubitDecls
      ++ outputDecls
      ++ [""]
      ++ concatMap (renderStmt 0) stmts
  where
    qubitDecls
      | qubitCount <= 0 = []
      | otherwise = ["qubit[" ++ show qubitCount ++ "] q;"]
    outputDecls =
      [ "bit output_" ++ show i ++ ";"
      | i <- [0 .. outputArity - 1]
      ]


renderStmt :: Int -> Stmt -> [String]
renderStmt indentLevel (StmtGate gateName [q]) =
  [indent indentLevel ++ gateName ++ " q[" ++ show q ++ "];"]
renderStmt indentLevel (StmtGate gateName [q0, q1]) =
  [indent indentLevel ++ gateName ++ " q[" ++ show q0 ++ "], q[" ++ show q1 ++ "];"]
renderStmt indentLevel (StmtMeasure bitName q zeroStmts oneStmts) =
  [ indent indentLevel ++ "bit " ++ bitName ++ " = measure q[" ++ show q ++ "];"
  , indent indentLevel ++ "if (" ++ bitName ++ " == 0) {"
  ]
    ++ concatMap (renderStmt (indentLevel + 1)) zeroStmts
    ++ [indent indentLevel ++ "} else {"]
    ++ concatMap (renderStmt (indentLevel + 1)) oneStmts
    ++ [indent indentLevel ++ "}"]
renderStmt indentLevel (StmtAssign name valueExpr) =
  [indent indentLevel ++ name ++ " = " ++ valueExpr ++ ";"]
renderStmt indentLevel (StmtDeclareAssign declType name valueExpr) =
  [indent indentLevel ++ declType ++ " " ++ name ++ " = " ++ valueExpr ++ ";"]
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
