module RecordShape
  ( Shape(..)
  , FunctionKey(..)
  , ModuleRecordShapes(..)
  , analyzeModuleRecordShapes
  , renderModuleRecordShapes
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map

import CPSExp
import Utils (Variable)


data Shape
  = ShapeUnknown
  | ShapeScalar
  | ShapeRecord [Shape]
  | ShapeOpaque
  deriving (Eq, Ord)


data FunctionKey
  = TopLevelFunction String
  | LocalFunction Variable
  | ParamFunction FunctionKey Int
  deriving (Eq, Ord)


data ModuleRecordShapes = ModuleRecordShapes
  { moduleFunctionShapes :: Map.Map FunctionKey [Shape]
  }


type InterfaceMap = Map.Map FunctionKey [Shape]
type Env = Map.Map Variable Shape
type Scope = Map.Map Variable FunctionKey


data ShapeDecl = ShapeDecl
  { shapeDeclName :: String
  , shapeDeclBody :: CExp
  }


analyzeModuleRecordShapes :: [(String, Either String CExp)] -> ModuleRecordShapes
analyzeModuleRecordShapes decls =
  let shapeDecls =
        [ ShapeDecl name body
        | (name, Right body) <- decls
        ]
      arities = collectModuleArities shapeDecls
      interfaces = iterateInterfaces arities shapeDecls
  in ModuleRecordShapes { moduleFunctionShapes = interfaces }


renderModuleRecordShapes :: ModuleRecordShapes -> [String]
renderModuleRecordShapes shapes =
  let entries = filter (isTopLevelEntry . fst) (sortedEntries (moduleFunctionShapes shapes))
  in case entries of
       [] -> ["  (no callable top-level functions inferred)"]
       _  -> map renderEntry entries


iterateInterfaces :: Map.Map FunctionKey Int -> [ShapeDecl] -> InterfaceMap
iterateInterfaces arities decls =
  let initInterfaces =
        Map.map (\arity -> replicate arity ShapeUnknown) arities
      step interfaces =
        mergeObservations arities
          (Map.unionsWith mergeShapeLists (map (analyzeDecl interfaces) decls))
      loop interfaces =
        let interfaces' = step interfaces
        in if interfaces' == interfaces
             then interfaces
             else loop interfaces'
  in loop initInterfaces


collectModuleArities :: [ShapeDecl] -> Map.Map FunctionKey Int
collectModuleArities =
  Map.unionsWith preferTopLevel . map collectDeclArities


preferTopLevel :: Int -> Int -> Int
preferTopLevel left _ = left


collectDeclArities :: ShapeDecl -> Map.Map FunctionKey Int
collectDeclArities decl =
  collectExpArities (exportedFunction decl) Map.empty (shapeDeclBody decl)


collectExpArities
  :: Maybe (Variable, String)
  -> Scope
  -> CExp
  -> Map.Map FunctionKey Int
collectExpArities exportAlias scope (CRecord _ _ body) =
  collectExpArities exportAlias scope body
collectExpArities exportAlias scope (CSelect _ _ _ body) =
  collectExpArities exportAlias scope body
collectExpArities exportAlias scope (COffset _ _ _ body) =
  collectExpArities exportAlias scope body
collectExpArities _ scope (CApp fn args) =
  case calleeKey scope fn of
    Just key -> Map.singleton key (length args)
    Nothing  -> Map.empty
collectExpArities _ scope (CSwitch _ arms) =
  Map.unionsWith preferTopLevel (map (collectExpArities Nothing scope) arms)
collectExpArities _ scope (CPrimOp _ _ _ conts) =
  Map.unionsWith preferTopLevel (map (collectExpArities Nothing scope) conts)
collectExpArities exportAlias scope (CFix defs body) =
  let localScope = bindFunctionKeys exportAlias defs scope
      localArities =
        Map.fromList
          [ (functionKeyFor exportAlias f, length params)
          | (f, params, _) <- defs
          ]
      defArities =
        Map.unionsWith preferTopLevel
          [ collectExpArities Nothing (bindParamKeys (functionKeyFor exportAlias f) params localScope) defBody
          | (f, params, defBody) <- defs
          ]
      bodyArities = collectExpArities Nothing localScope body
  in Map.unionsWith preferTopLevel [localArities, defArities, bodyArities]


analyzeDecl :: InterfaceMap -> ShapeDecl -> InterfaceMap
analyzeDecl interfaces decl =
  analyzeExp interfaces
    (exportedFunction decl)
    Map.empty
    Map.empty
    (shapeDeclBody decl)


analyzeExp
  :: InterfaceMap
  -> Maybe (Variable, String)
  -> Scope
  -> Env
  -> CExp
  -> InterfaceMap
analyzeExp interfaces exportAlias scope env (CRecord fields v body) =
  let env' = Map.insert v (ShapeRecord (map (fieldShape env) fields)) env
  in analyzeExp interfaces exportAlias scope env' body

analyzeExp interfaces exportAlias scope env (CSelect i val v body) =
  let env' = Map.insert v (selectShape i (valueShape env val)) env
  in analyzeExp interfaces exportAlias scope env' body

analyzeExp interfaces exportAlias scope env (COffset off val v body) =
  let env' = Map.insert v (offsetShape off (valueShape env val)) env
  in analyzeExp interfaces exportAlias scope env' body

analyzeExp interfaces _ scope env (CApp fn args) =
  case calleeKey scope fn of
    Just key ->
      Map.unionsWith mergeShapeLists
        (Map.singleton key (map (valueShape env) args)
        : [ bridgeCallableArg interfaces scope key index arg
          | (index, arg) <- zip [0 :: Int ..] args
          ])
    Nothing  -> Map.empty

analyzeExp interfaces exportAlias scope env (CFix defs body) =
  let localScope = bindFunctionKeys exportAlias defs scope
      envWithFns = bindFunctionValues localScope env
      defObs =
        Map.unionsWith mergeShapeLists
          [ analyzeDef interfaces localScope envWithFns exportAlias def
          | def <- defs
          ]
      bodyObs = analyzeExp interfaces Nothing localScope envWithFns body
  in Map.unionWith mergeShapeLists defObs bodyObs

analyzeExp interfaces _ scope env (CSwitch _ arms) =
  Map.unionsWith mergeShapeLists
    [ analyzeExp interfaces Nothing scope env arm
    | arm <- arms
    ]

analyzeExp interfaces _ scope env (CPrimOp op _args results conts) =
  let env' = foldr (\v acc -> Map.insert v (resultShape op) acc) env results
  in Map.unionsWith mergeShapeLists
       [ analyzeExp interfaces Nothing scope env' cont
       | cont <- conts
       ]


analyzeDef
  :: InterfaceMap
  -> Scope
  -> Env
  -> Maybe (Variable, String)
  -> (Variable, [Variable], CExp)
  -> InterfaceMap
analyzeDef interfaces scope env exportAlias (f, params, body) =
  let key = functionKeyFor exportAlias f
      paramShapes = Map.findWithDefault (replicate (length params) ShapeUnknown) key interfaces
      scope' = bindParamKeys key params scope
      env' =
        foldr (\(param, shape) acc -> Map.insert param shape acc)
          env
          (zip params (padTo (length params) ShapeUnknown paramShapes))
  in analyzeExp interfaces Nothing scope' env' body


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


bindFunctionValues :: Scope -> Env -> Env
bindFunctionValues scope env =
  foldr Map.delete env (Map.keys scope)
  `Map.union`
  Map.fromList [ (v, ShapeScalar) | v <- Map.keys scope ]


bridgeCallableArg :: InterfaceMap -> Scope -> FunctionKey -> Int -> Value -> InterfaceMap
bridgeCallableArg interfaces scope callee index arg =
  case calleeKey scope arg of
    Just actual ->
      let formal = ParamFunction callee index
      in case Map.lookup formal interfaces of
           Just formalShapes
             | not (null formalShapes) ->
                 Map.singleton actual formalShapes
           _ ->
                 Map.empty
    Nothing ->
      Map.empty


exportedFunction :: ShapeDecl -> Maybe (Variable, String)
exportedFunction decl =
  case shapeDeclBody decl of
    CFix defs (CApp (VLabel "halt") [VVar exported])
      | exported `elem` [ f | (f, _, _) <- defs ] ->
          Just (exported, shapeDeclName decl)
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


fieldShape :: Env -> (Value, AccessPath) -> Shape
fieldShape env (val, path) =
  applyPathShape path (valueShape env val)


valueShape :: Env -> Value -> Shape
valueShape env (VVar v) =
  Map.findWithDefault ShapeUnknown v env
valueShape _ _ =
  ShapeScalar


resultShape :: a -> Shape
resultShape _ = ShapeScalar


applyPathShape :: AccessPath -> Shape -> Shape
applyPathShape (OFFp 0) shape =
  shape
applyPathShape (OFFp n) (ShapeRecord fields)
  | n >= 0
  , n < length fields =
      ShapeRecord (drop n fields)
applyPathShape (OFFp _) ShapeUnknown =
  ShapeUnknown
applyPathShape (OFFp _) _ =
  ShapeOpaque
applyPathShape (SELp i rest) (ShapeRecord fields)
  | i >= 0
  , i < length fields =
      applyPathShape rest (fields !! i)
applyPathShape (SELp _ _) ShapeUnknown =
  ShapeUnknown
applyPathShape (SELp _ _) _ =
  ShapeOpaque


selectShape :: Int -> Shape -> Shape
selectShape i (ShapeRecord fields)
  | i >= 0
  , i < length fields =
      fields !! i
selectShape _ ShapeUnknown =
  ShapeUnknown
selectShape _ _ =
  ShapeOpaque


offsetShape :: Int -> Shape -> Shape
offsetShape off (ShapeRecord fields)
  | off >= 0
  , off < length fields =
      ShapeRecord (drop off fields)
offsetShape _ ShapeUnknown =
  ShapeUnknown
offsetShape _ _ =
  ShapeOpaque


mergeObservations :: Map.Map FunctionKey Int -> InterfaceMap -> InterfaceMap
mergeObservations arities observations =
  Map.mapWithKey mergeOne observations
  where
    mergeOne key shapes =
      let arity = Map.findWithDefault (length shapes) key arities
      in padTo arity ShapeUnknown shapes


mergeShapeLists :: [Shape] -> [Shape] -> [Shape]
mergeShapeLists left right =
  zipWith mergeShape (padTo width ShapeUnknown left) (padTo width ShapeUnknown right)
  where
    width = max (length left) (length right)


mergeShape :: Shape -> Shape -> Shape
mergeShape ShapeUnknown shape =
  shape
mergeShape shape ShapeUnknown =
  shape
mergeShape ShapeOpaque _ =
  ShapeOpaque
mergeShape _ ShapeOpaque =
  ShapeOpaque
mergeShape ShapeScalar ShapeScalar =
  ShapeScalar
mergeShape (ShapeRecord left) (ShapeRecord right)
  | length left == length right =
      ShapeRecord (zipWith mergeShape left right)
mergeShape (ShapeRecord _) (ShapeRecord _) =
  ShapeOpaque
mergeShape _ _ =
  ShapeOpaque


padTo :: Int -> a -> [a] -> [a]
padTo n filler xs =
  take n (xs ++ repeat filler)


sortedEntries :: InterfaceMap -> [(FunctionKey, [Shape])]
sortedEntries =
  List.sortOn (renderKey . fst) . Map.toList


renderEntry :: (FunctionKey, [Shape]) -> String
renderEntry (key, shapes) =
  "  " ++ renderKey key ++ " : " ++ renderShapeList shapes


isTopLevelEntry :: FunctionKey -> Bool
isTopLevelEntry (TopLevelFunction _) = True
isTopLevelEntry _                    = False


renderKey :: FunctionKey -> String
renderKey (TopLevelFunction name) =
  name
renderKey (LocalFunction v) =
  show v
renderKey (ParamFunction key index) =
  renderKey key ++ "#" ++ show index


renderShapeList :: [Shape] -> String
renderShapeList shapes =
  "(" ++ List.intercalate ", " (map renderShape shapes) ++ ")"


renderShape :: Shape -> String
renderShape ShapeUnknown =
  "?"
renderShape ShapeScalar =
  "scalar"
renderShape ShapeOpaque =
  "opaque"
renderShape (ShapeRecord fields) =
  "record[" ++ List.intercalate ", " (map renderShape fields) ++ "]"
