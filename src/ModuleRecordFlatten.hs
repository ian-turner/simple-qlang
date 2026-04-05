module ModuleRecordFlatten
  ( flattenModuleRecordInterfaces
  ) where

import qualified Data.Map.Strict as Map

import CPSExp
import CPSAtom
import RecordShape
import Utils (Variable, freshNames)


type Scope = Map.Map Variable FunctionKey


flattenModuleRecordInterfaces :: ModuleRecordShapes -> String -> CExp -> CExp
flattenModuleRecordInterfaces shapes declName body =
  flattenExp shapes (exportedFunction declName body) Map.empty Map.empty body


flattenExp
  :: ModuleRecordShapes
  -> Maybe (Variable, String)
  -> Scope
  -> Env
  -> CExp
  -> CExp
flattenExp shapes exportAlias scope env (CRecord fields v body) =
  let fields' = map (rewriteField env) fields
      env' = Map.delete v env
      body' =
        case recordAtom env fields' of
          Just atom -> flattenExp shapes exportAlias scope (Map.insert v atom env') body
          Nothing   -> flattenExp shapes exportAlias scope env' body
  in if occursInExp v body'
       then CRecord fields' v body'
       else body'

flattenExp shapes exportAlias scope env (CSelect i val v body) =
  let val' = rewriteValue env val
      env' = Map.delete v env
  in case selectAtom env val' i of
       Just atom ->
         let body' = flattenExp shapes exportAlias scope (Map.insert v atom env') body
         in if occursInExp v body'
              then CSelect i val' v body'
              else body'
       Nothing ->
         CSelect i val' v (flattenExp shapes exportAlias scope env' body)

flattenExp shapes exportAlias scope env (COffset off val v body) =
  let val' = rewriteValue env val
      env' = Map.delete v env
  in case offsetAtom env val' off of
       Just atom ->
         let body' = flattenExp shapes exportAlias scope (Map.insert v atom env') body
         in if occursInExp v body'
              then COffset off val' v body'
              else body'
       Nothing ->
         COffset off val' v (flattenExp shapes exportAlias scope env' body)

flattenExp shapes _ scope env (CApp fn args) =
  let fn' = rewriteValue env fn
      argShapes = calleeShapes shapes scope fn'
  in CApp fn' (rewriteArgs env argShapes args)

flattenExp shapes exportAlias scope env (CFix defs body) =
  let localScope = bindFunctionKeys exportAlias defs scope
      env' = foldr Map.delete env [ f | (f, _, _) <- defs ]
      defs' = map (flattenDef shapes exportAlias localScope env') defs
  in CFix defs' (flattenExp shapes Nothing localScope env' body)

flattenExp shapes _ scope env (CSwitch val arms) =
  CSwitch (rewriteValue env val) (map (flattenExp shapes Nothing scope env) arms)

flattenExp shapes _ scope env (CPrimOp op args results conts) =
  let env' = foldr Map.delete env results
  in CPrimOp op
       (map (rewriteValue env) args)
       results
       (map (flattenExp shapes Nothing scope env') conts)

flattenExp shapes _ scope env (CFor idx lo hi body cont) =
  let lo'   = rewriteValue env lo
      hi'   = rewriteValue env hi
      env'  = Map.delete idx env
      body' = flattenExp shapes Nothing scope env' body
      cont' = flattenExp shapes Nothing scope env cont
  in CFor idx lo' hi' body' cont'


flattenDef
  :: ModuleRecordShapes
  -> Maybe (Variable, String)
  -> Scope
  -> Env
  -> (Variable, [Variable], CExp)
  -> (Variable, [Variable], CExp)
flattenDef shapes exportAlias scope env (f, params, body) =
  let key = functionKeyFor exportAlias f
      paramShapes = lookupParamShapes shapes key (length params)
      (params', paramEnv) = flattenParams params paramShapes
      scope' = bindParamKeys key params scope
      env' =
        Map.union paramEnv $
          foldr Map.delete (Map.delete f env) params
  in (f, params', flattenExp shapes Nothing scope' env' body)


flattenParams :: [Variable] -> [Shape] -> ([Variable], Env)
flattenParams params shapes =
  let flattened = zipWith flattenOne params (padTo (length params) ShapeUnknown shapes)
  in (concatMap fst flattened, Map.unions (map snd flattened))


flattenOne :: Variable -> Shape -> ([Variable], Env)
flattenOne param shape =
  case flattenableRecordShape shape of
    Just recordShape ->
      let hints = leafNameHints (show param) recordShape
      in freshNames hints $ \vars ->
           let (atom, rest) = buildAtom recordShape vars
           in case rest of
                [] -> (vars, Map.singleton param atom)
                _  -> ([param], Map.empty)
    Nothing ->
      ([param], Map.empty)


flattenableRecordShape :: Shape -> Maybe Shape
flattenableRecordShape shape@(ShapeRecord fields)
  | all scalarizableShape fields = Just shape
flattenableRecordShape _ =
  Nothing


scalarizableShape :: Shape -> Bool
scalarizableShape ShapeScalar = True
scalarizableShape (ShapeRecord fields) = all scalarizableShape fields
scalarizableShape _ = False


leafNameHints :: String -> Shape -> [String]
leafNameHints base shape =
  [ base ++ "_" ++ suffix
  | suffix <- leafSuffixes shape
  ]


leafSuffixes :: Shape -> [String]
leafSuffixes ShapeScalar = ["0"]
leafSuffixes (ShapeRecord fields) =
  concat
    [ [ show i ++ "_" ++ suffix
      | suffix <- leafSuffixes field
      ]
    | (i, field) <- zip [0 :: Int ..] fields
    ]
leafSuffixes _ = []


buildAtom :: Shape -> [Variable] -> (Atom, [Variable])
buildAtom ShapeScalar (v : vs) =
  (AScalar (VVar v), vs)
buildAtom (ShapeRecord fields) vars =
  let (atoms, rest) = buildAtoms fields vars
  in (ARecord atoms, rest)
buildAtom _ vars =
  (ARecord [], vars)


buildAtoms :: [Shape] -> [Variable] -> ([Atom], [Variable])
buildAtoms [] vars = ([], vars)
buildAtoms (shape : shapes) vars =
  let (atom, vars') = buildAtom shape vars
      (atoms, vars'') = buildAtoms shapes vars'
  in (atom : atoms, vars'')


rewriteArgs :: Env -> [Shape] -> [Value] -> [Value]
rewriteArgs env [] args =
  map (rewriteValue env) args
rewriteArgs _ _ [] =
  []
rewriteArgs env (shape : shapes) (arg : args) =
  rewriteArg env shape arg ++ rewriteArgs env shapes args


rewriteArg :: Env -> Shape -> Value -> [Value]
rewriteArg env shape arg =
  let arg' = rewriteValue env arg
  in case flattenableRecordShape shape of
       Just recordShape ->
         case flattenValue env arg' recordShape of
           Just values -> values
           Nothing     -> [arg']
       Nothing ->
         [arg']


flattenValue :: Env -> Value -> Shape -> Maybe [Value]
flattenValue env val shape =
  flattenAtomValues shape =<< atomOfValue env val


flattenAtomValues :: Shape -> Atom -> Maybe [Value]
flattenAtomValues ShapeScalar (AScalar val) =
  Just [val]
flattenAtomValues (ShapeRecord shapes) (ARecord atoms)
  | length shapes == length atoms =
      concat <$> zipWithM flattenAtomValues shapes atoms
flattenAtomValues _ _ =
  Nothing


zipWithM :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithM _ [] [] = Just []
zipWithM f (x : xs) (y : ys) = do
  z <- f x y
  zs <- zipWithM f xs ys
  pure (z : zs)
zipWithM _ _ _ = Nothing


lookupParamShapes :: ModuleRecordShapes -> FunctionKey -> Int -> [Shape]
lookupParamShapes shapes key arity =
  padTo arity ShapeUnknown $
    Map.findWithDefault (replicate arity ShapeUnknown) key (moduleFunctionShapes shapes)


calleeShapes :: ModuleRecordShapes -> Scope -> Value -> [Shape]
calleeShapes shapes scope fn =
  case calleeKey scope fn of
    Just key -> Map.findWithDefault [] key (moduleFunctionShapes shapes)
    Nothing  -> []


bindFunctionKeys
  :: Maybe (Variable, String)
  -> [(Variable, [Variable], CExp)]
  -> Scope
  -> Scope
bindFunctionKeys exportAlias defs scope =
  Map.fromList
    [ (f, functionKeyFor exportAlias f)
    | (f, _, _) <- defs
    ] `Map.union` scope


bindParamKeys :: FunctionKey -> [Variable] -> Scope -> Scope
bindParamKeys key params scope =
  Map.fromList
    [ (param, ParamFunction key index)
    | (index, param) <- zip [0 :: Int ..] params
    ] `Map.union` scope


exportedFunction :: String -> CExp -> Maybe (Variable, String)
exportedFunction declName body =
  case body of
    CFix defs (CApp (VLabel "halt") [VVar exported])
      | exported `elem` [ f | (f, _, _) <- defs ] ->
          Just (exported, declName)
    _ -> Nothing


functionKeyFor :: Maybe (Variable, String) -> Variable -> FunctionKey
functionKeyFor (Just (exported, name)) f
  | exported == f = TopLevelFunction name
functionKeyFor _ f =
  LocalFunction f


calleeKey :: Scope -> Value -> Maybe FunctionKey
calleeKey scope (VVar v) =
  Map.lookup v scope
calleeKey _ (VLabel "halt") =
  Nothing
calleeKey _ (VLabel name) =
  Just (TopLevelFunction name)
calleeKey _ _ =
  Nothing


padTo :: Int -> a -> [a] -> [a]
padTo n filler xs =
  take n (xs ++ repeat filler)
