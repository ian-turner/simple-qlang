module StaticShape
  ( Size(..)
  , Shape(..)
  , ModuleShapes(..)
  , analyzeModuleShapes
  , lookupTopLevelShape
  , applyShape
  , listSize
  ) where

import qualified Data.Map.Strict as Map

import LambdaIR
import Utils (Variable)


-- -----------------------------------------------------------------------
-- Data types

-- | Size of a homogeneous list.
--   SizeVar v means: if v is an Int parameter, size equals its value;
--                    if v is a list parameter, size equals its length.
data Size
  = SizeKnown Int
  | SizeVar   Variable
  | SizeUnknown
  deriving (Eq, Ord, Show)

-- | Static shape of a value.
--   ShapeFun v ret represents a single-argument function: applying it to an
--   argument instantiates SizeVar v references in ret with the argument's size.
data Shape
  = ShapeScalar               -- Int, Bool, or unit (undifferentiated)
  | ShapeQubit                -- quantum bit
  | ShapeBool                 -- classical measurement result
  | ShapeTuple [Shape]        -- fixed-size heterogeneous tuple
  | ShapeList  Size Shape     -- homogeneous list with known or parametric size
  | ShapeFun   Variable Shape -- function: param variable + return shape
  | ShapeUnknown
  deriving (Eq, Ord, Show)

-- | Whole-module static shapes: maps each top-level name to its shape.
newtype ModuleShapes = ModuleShapes
  { topLevelShapes :: Map.Map String Shape
  }


-- -----------------------------------------------------------------------
-- Top-level API

analyzeModuleShapes :: [(String, LExp)] -> ModuleShapes
analyzeModuleShapes decls = ModuleShapes (converge (12 :: Int) Map.empty)
  where
    step m = foldr (\(name, expr) acc ->
               Map.insert name (inferDeclShape m name expr) acc) m decls
    converge 0 m = m
    converge n m = let m' = step m in if m' == m then m else converge (n-1) m'

lookupTopLevelShape :: ModuleShapes -> String -> Shape
lookupTopLevelShape (ModuleShapes m) name =
  Map.findWithDefault ShapeUnknown name m

-- | Apply a ShapeFun to an argument, substituting SizeVar references.
--   argShape: inferred shape of the argument value
--   argInt:   concrete integer value if known (for Int params)
applyShape :: Shape -> Shape -> Maybe Int -> Shape
applyShape (ShapeFun v ret) argShape argInt =
  substituteVar v argShape argInt ret
applyShape _ _ _ = ShapeUnknown

-- | Extract the size from a ShapeList, if present.
listSize :: Shape -> Maybe Size
listSize (ShapeList s _) = Just s
listSize _               = Nothing


-- -----------------------------------------------------------------------
-- Declaration analysis

inferDeclShape :: Map.Map String Shape -> String -> LExp -> Shape
inferDeclShape topEnv name (LLam v body) =
  ShapeFun v (inferBodyShape topEnv name v body)
inferDeclShape topEnv _ expr =
  inferExprShape topEnv Map.empty expr


-- -----------------------------------------------------------------------
-- Body analysis: try specific patterns before falling back to general inference

inferBodyShape
  :: Map.Map String Shape
  -> String    -- name of the enclosing function (for self-call detection)
  -> Variable  -- outermost lambda parameter
  -> LExp      -- body of the lambda
  -> Shape
inferBodyShape topEnv name param body =
  case recognizeCountdown topEnv name param body of
    Just shape -> shape
    Nothing ->
      case recognizeListMap topEnv name param body of
        Just shape -> shape
        Nothing    -> inferExprShape topEnv Map.empty body


-- -----------------------------------------------------------------------
-- General expression shape inference

type LocalEnv  = Map.Map Variable Shape
type BindMap   = Map.Map Variable LExp   -- tracks let-bound variable → definition

inferExprShape :: Map.Map String Shape -> LocalEnv -> LExp -> Shape
inferExprShape _      _     (LLit (LInt _))    = ShapeScalar
inferExprShape _      _     (LLit (LFloat _))  = ShapeScalar
inferExprShape _      _     (LLit (LBool _))   = ShapeScalar
inferExprShape _      _     (LLit LUnit)       = ShapeScalar
inferExprShape _      _     (LLit (LString _)) = ShapeScalar
inferExprShape _      local (LVar v)           = Map.findWithDefault ShapeUnknown v local
inferExprShape topEnv _     (LTopVar name)     = Map.findWithDefault ShapeUnknown name topEnv

inferExprShape _  _ (LPrim PInit  _) = ShapeQubit
inferExprShape _  _ (LPrim PMeas  _) = ShapeBool
inferExprShape _  _ (LPrim PHGate _) = ShapeQubit
inferExprShape _  _ (LPrim PXGate _) = ShapeQubit
inferExprShape _  _ (LPrim PZGate _) = ShapeQubit
inferExprShape _  _ (LPrim PSGate _) = ShapeQubit
inferExprShape _  _ (LPrim PTGate _) = ShapeQubit
inferExprShape _  _ (LPrim PCNot   _) = ShapeTuple [ShapeQubit, ShapeQubit]
inferExprShape _  _ (LPrim PCSGate _) = ShapeTuple [ShapeQubit, ShapeQubit]
inferExprShape _  _ (LPrim PCTGate _) = ShapeTuple [ShapeQubit, ShapeQubit]
inferExprShape _  _ (LPrim PCpGate _) = ShapeTuple [ShapeQubit, ShapeQubit]
inferExprShape _  _ (LPrim PAdd  _)  = ShapeScalar
inferExprShape _  _ (LPrim PSub  _)  = ShapeScalar
inferExprShape _  _ (LPrim PMul  _)  = ShapeScalar
inferExprShape _  _ (LPrim PDiv  _)  = ShapeScalar
inferExprShape _  _ (LPrim PEq   _)  = ShapeScalar
inferExprShape _  _ (LPrim PLt   _)  = ShapeScalar
inferExprShape _  _ (LPrim PGt   _)  = ShapeScalar
inferExprShape _  _ (LPrim PLe   _)  = ShapeScalar
inferExprShape _  _ (LPrim PGe   _)  = ShapeScalar
inferExprShape _  _ (LPrim PAnd  _)  = ShapeScalar
inferExprShape _  _ (LPrim POr   _)  = ShapeScalar
inferExprShape _  _ (LPrim PNot  _)  = ShapeScalar

inferExprShape topEnv local (LTuple es) =
  ShapeTuple (map (inferExprShape topEnv local) es)

inferExprShape topEnv local (LSelect i e) =
  case inferExprShape topEnv local e of
    ShapeTuple ss | i >= 0, i < length ss -> ss !! i
    _                                      -> ShapeUnknown

inferExprShape _ _ (LCon "Nil" _) =
  ShapeList (SizeKnown 0) ShapeUnknown

inferExprShape topEnv local (LCon "Cons" payload) =
  let (elemS, tailS) = case payload of
        LTuple [h, t] -> (inferExprShape topEnv local h, inferExprShape topEnv local t)
        _             -> (ShapeUnknown, ShapeUnknown)
      newSize = case tailS of
        ShapeList (SizeKnown n) _ -> SizeKnown (n + 1)
        _                         -> SizeUnknown
  in ShapeList newSize elemS

inferExprShape _ _ (LCon _ _) = ShapeUnknown

inferExprShape topEnv local (LDecon _ e) =
  case inferExprShape topEnv local e of
    ShapeTuple (s : _) -> s
    _                  -> ShapeUnknown

inferExprShape topEnv local (LSwitch _ arms mbDef) =
  let armShapes = [inferExprShape topEnv local body | (_, body) <- arms]
      defShapes = maybe [] (\d -> [inferExprShape topEnv local d]) mbDef
  in foldr mergeShape ShapeUnknown (armShapes ++ defShapes)

-- Let-binding: LApp (LLam v body) arg
inferExprShape topEnv local (LApp (LLam v body) arg) =
  let argShape = inferExprShape topEnv local arg
      local'   = Map.insert v argShape local
  in inferExprShape topEnv local' body

-- General application
inferExprShape topEnv local (LApp f arg) =
  let fShape   = inferExprShape topEnv local f
      argShape = inferExprShape topEnv local arg
      argInt   = evalIntLit arg
  in applyShape fShape argShape argInt

inferExprShape topEnv local (LLam v body) =
  ShapeFun v (inferExprShape topEnv (Map.insert v ShapeUnknown local) body)

inferExprShape topEnv local (LFix _ body) =
  inferExprShape topEnv local body


-- -----------------------------------------------------------------------
-- Pattern recognizer: countdown list constructor
--
-- f(param) = if param == 0 then Nil else Cons elem (f (param - 1))
--
-- Lowered form: LSwitch (LPrim PEq [LVar param, LLit 0])
--                 [(CACon "True", Nil), (CACon "False", Cons elem (f _))]

recognizeCountdown
  :: Map.Map String Shape
  -> String    -- function being defined
  -> Variable  -- counter parameter
  -> LExp
  -> Maybe Shape
recognizeCountdown _ name param body =
  case body of
    LSwitch cond [(CACon trueName, trueArm), (CACon _, falseArm)] Nothing
      | isCounterEqZero param cond ->
          let (baseArm, recArm) = if trueName == "True"
                                    then (trueArm, falseArm)
                                    else (falseArm, trueArm)
          in check baseArm recArm
    _ -> Nothing
  where
    check baseArm recArm
      | isNilExp baseArm
      , Just elemShape <- extractConsHead name Map.empty recArm
      = Just (ShapeList (SizeVar param) elemShape)
      | otherwise = Nothing


-- -----------------------------------------------------------------------
-- Pattern recognizer: structural list map (same-size recursion)
--
-- f(param) = case param of Nil -> Nil; Cons h t -> Cons (g h) (f t)
--
-- Lowered form:
--   LApp (LLam t (LSwitch (LVar t)
--     [(CACon "Nil",  nil_body),
--      (CACon "Cons", cons_body)] Nothing)) (LVar param)

recognizeListMap
  :: Map.Map String Shape
  -> String
  -> Variable  -- list parameter
  -> LExp
  -> Maybe Shape
recognizeListMap _ name param body =
  case body of
    LApp (LLam t (LSwitch (LVar t') arms Nothing)) (LVar p)
      | t == t', p == param
      , Just nilArm  <- lookup (CACon "Nil")  arms
      , Just consArm <- lookup (CACon "Cons") arms
      , isNilExp nilArm ->
          case extractConsHead name Map.empty consArm of
            Just elemShape -> Just (ShapeList (SizeVar param) elemShape)
            Nothing        -> Nothing
    _ -> Nothing


-- -----------------------------------------------------------------------
-- Shared helpers for the pattern recognizers

isCounterEqZero :: Variable -> LExp -> Bool
isCounterEqZero v (LPrim PEq [LVar u, LLit (LInt 0)]) = u == v
isCounterEqZero v (LPrim PEq [LLit (LInt 0), LVar u]) = u == v
isCounterEqZero _ _                                    = False

isNilExp :: LExp -> Bool
isNilExp (LCon "Nil" _) = True
isNilExp _              = False

-- | Walk a let-binding chain and find `Cons h tail` where the tail contains
--   a self-call (direct or via a bound variable).  Returns the inferred shape
--   of the head element.
--
--   `bindings` tracks let-bound variables so that
--   `let rest = f xs' in Cons q rest` is correctly recognized.
extractConsHead :: String -> BindMap -> LExp -> Maybe Shape
extractConsHead name bindings (LCon "Cons" (LTuple [h, t]))
  | hasSelfCall name bindings t = Just (elemShapeOf bindings h)
  | otherwise                   = Nothing
extractConsHead name bindings (LApp (LLam v body) rhs) =
  extractConsHead name (Map.insert v rhs bindings) body
extractConsHead name bindings (LVar v)
  | Just e <- Map.lookup v bindings = extractConsHead name bindings e
extractConsHead _ _ _ = Nothing

-- | Check whether an expression (or a variable it's bound to) contains a
--   call to the named function.
hasSelfCall :: String -> BindMap -> LExp -> Bool
hasSelfCall name _        (LApp (LTopVar n) _) = n == name
hasSelfCall name bindings (LVar v)
  | Just e <- Map.lookup v bindings = hasSelfCall name bindings e
hasSelfCall name bindings (LApp (LLam _ body) _) = hasSelfCall name bindings body
hasSelfCall _ _ _                                 = False

-- | Infer the element shape from the head of a Cons.
--   Resolves variable references through `bindings`.
elemShapeOf :: BindMap -> LExp -> Shape
elemShapeOf bindings (LVar v)
  | Just e <- Map.lookup v bindings = elemShapeOf bindings e
elemShapeOf _ (LPrim PInit  _) = ShapeQubit
elemShapeOf _ (LPrim PMeas  _) = ShapeBool
elemShapeOf _ (LPrim PHGate _) = ShapeQubit
elemShapeOf _ (LPrim PXGate _) = ShapeQubit
elemShapeOf _ (LPrim PZGate _) = ShapeQubit
elemShapeOf _ (LPrim PSGate _) = ShapeQubit
elemShapeOf _ (LPrim PTGate _) = ShapeQubit
elemShapeOf _ _                = ShapeUnknown


-- -----------------------------------------------------------------------
-- Shape operations

mergeShape :: Shape -> Shape -> Shape
mergeShape ShapeUnknown s = s
mergeShape s ShapeUnknown = s
mergeShape ShapeScalar ShapeScalar   = ShapeScalar
mergeShape ShapeQubit  ShapeQubit    = ShapeQubit
mergeShape ShapeBool   ShapeBool     = ShapeBool
mergeShape (ShapeTuple ss1) (ShapeTuple ss2)
  | length ss1 == length ss2 = ShapeTuple (zipWith mergeShape ss1 ss2)
mergeShape (ShapeList s1 e1) (ShapeList s2 e2) =
  ShapeList (mergeSize s1 s2) (mergeShape e1 e2)
mergeShape (ShapeFun _ _) (ShapeFun _ _) = ShapeUnknown  -- can't merge distinct functions
mergeShape _ _ = ShapeUnknown

mergeSize :: Size -> Size -> Size
mergeSize SizeUnknown s = s
mergeSize s SizeUnknown = s
mergeSize (SizeKnown m) (SizeKnown n)
  | m == n    = SizeKnown m
  | otherwise = SizeUnknown
mergeSize (SizeVar u) (SizeVar v)
  | u == v    = SizeVar u
  | otherwise = SizeUnknown
mergeSize _ _ = SizeUnknown

-- | Substitute all SizeVar v occurrences in a shape.
--   argShape: the shape of the argument bound to v
--   argInt:   concrete integer value for v, if known
substituteVar :: Variable -> Shape -> Maybe Int -> Shape -> Shape
substituteVar v argShape argInt = go
  where
    go ShapeUnknown     = ShapeUnknown
    go ShapeScalar      = ShapeScalar
    go ShapeQubit       = ShapeQubit
    go ShapeBool        = ShapeBool
    go (ShapeTuple ss)  = ShapeTuple (map go ss)
    go (ShapeList (SizeVar u) e)
      | u == v =
          let newSize = case argInt of
                          Just n  -> SizeKnown n
                          Nothing -> case argShape of
                                       ShapeList s _ -> s
                                       _             -> SizeUnknown
          in ShapeList newSize (go e)
    go (ShapeList s e) = ShapeList s (go e)
    go (ShapeFun u ret)
      | u /= v    = ShapeFun u (go ret)  -- v is free in ret; substitute through
      | otherwise = ShapeFun u ret       -- u == v: u shadows v, don't substitute

evalIntLit :: LExp -> Maybe Int
evalIntLit (LLit (LInt n)) = Just n
evalIntLit _               = Nothing
