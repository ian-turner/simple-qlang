module ModuleRecordFlatten
  ( flattenModuleRecordInterfaces
  ) where

import qualified Data.Map.Strict as Map

import CPSExp
import RecordShape
import Utils (Variable, freshNames)


data Atom
  = AScalar Value
  | ARecord [Atom]

type Env = Map.Map Variable Atom
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
      env' =
        Map.union paramEnv $
          foldr Map.delete (Map.delete f env) params
  in (f, params', flattenExp shapes Nothing scope env' body)


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


recordAtom :: Env -> [(Value, AccessPath)] -> Maybe Atom
recordAtom env fields =
  ARecord <$> mapM (fieldAtom env) fields


fieldAtom :: Env -> (Value, AccessPath) -> Maybe Atom
fieldAtom env (val, path) =
  applyPath path =<< atomOfValue env val


atomOfValue :: Env -> Value -> Maybe Atom
atomOfValue env (VVar v) =
  Just (Map.findWithDefault (AScalar (VVar v)) v env)
atomOfValue _ val =
  Just (AScalar val)


applyPath :: AccessPath -> Atom -> Maybe Atom
applyPath (OFFp 0) atom = Just atom
applyPath (OFFp n) (ARecord fields)
  | n >= 0
  , n < length fields =
      Just (ARecord (drop n fields))
applyPath (OFFp _) _ = Nothing
applyPath (SELp i rest) (ARecord fields)
  | i >= 0
  , i < length fields =
      applyPath rest (fields !! i)
applyPath (SELp _ _) _ = Nothing


selectAtom :: Env -> Value -> Int -> Maybe Atom
selectAtom env (VVar v) i = do
  atom <- Map.lookup v env
  case atom of
    ARecord fields
      | i >= 0
      , i < length fields ->
          Just (fields !! i)
    _ -> Nothing
selectAtom _ _ _ = Nothing


offsetAtom :: Env -> Value -> Int -> Maybe Atom
offsetAtom env (VVar v) off = do
  atom <- Map.lookup v env
  case atom of
    ARecord fields
      | off >= 0
      , off < length fields ->
          Just (ARecord (drop off fields))
    _ -> Nothing
offsetAtom _ _ _ = Nothing


rewriteField :: Env -> (Value, AccessPath) -> (Value, AccessPath)
rewriteField env (val, path) =
  (rewriteValue env val, path)


rewriteValue :: Env -> Value -> Value
rewriteValue env val@(VVar v) =
  case Map.lookup v env of
    Just (AScalar val') -> val'
    _                   -> val
rewriteValue _ val = val


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


occursInExp :: Variable -> CExp -> Bool
occursInExp v (CRecord fields bound body) =
  occursInFields v fields || (v /= bound && occursInExp v body)
occursInExp v (CSelect _ val bound body) =
  occursInValue v val || (v /= bound && occursInExp v body)
occursInExp v (COffset _ val bound body) =
  occursInValue v val || (v /= bound && occursInExp v body)
occursInExp v (CApp fn args) =
  any (occursInValue v) (fn : args)
occursInExp v (CFix defs body) =
  let bound = map (\(f, _, _) -> f) defs
      inDefs = any (occursInDef v bound) defs
      inBody = if v `elem` bound then False else occursInExp v body
  in inDefs || inBody
occursInExp v (CSwitch val arms) =
  occursInValue v val || any (occursInExp v) arms
occursInExp v (CPrimOp _ args results conts) =
  any (occursInValue v) args
  || if v `elem` results then False else any (occursInExp v) conts


occursInDef :: Variable -> [Variable] -> (Variable, [Variable], CExp) -> Bool
occursInDef v names (_f, params, body)
  | v `elem` names = False
  | v `elem` params = False
  | otherwise = occursInExp v body


occursInFields :: Variable -> [(Value, AccessPath)] -> Bool
occursInFields v =
  any (\(val, _) -> occursInValue v val)


occursInValue :: Variable -> Value -> Bool
occursInValue v (VVar x) = v == x
occursInValue _ _        = False
