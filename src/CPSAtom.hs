-- | Shared atom/environment utilities used by record-flattening passes.
--
-- Both 'RecordFlatten' and 'ModuleRecordFlatten' need the same atom
-- representation, occurrence-checking, and value-rewriting helpers.  They
-- live here so a bug fix or extension only needs to happen once.
module CPSAtom
  ( Atom(..)
  , Env
  , atomOfValue
  , applyPath
  , selectAtom
  , offsetAtom
  , recordAtom
  , fieldAtom
  , rewriteField
  , rewriteValue
  , occursInExp
  , occursInDef
  , occursInFields
  , occursInValue
  ) where

import qualified Data.Map.Strict as Map

import CPSExp
import Utils (Variable)


-- | A symbolic atom: either a single 'Value' or a statically-known record.
data Atom
  = AScalar Value
  | ARecord [Atom]


-- | Map from variables to their known atom shapes.
type Env = Map.Map Variable Atom


-- ---------------------------------------------------------------------------
-- Atom construction from values and access paths
-- ---------------------------------------------------------------------------

atomOfValue :: Env -> Value -> Maybe Atom
atomOfValue env (VVar v) =
  Just $ Map.findWithDefault (AScalar (VVar v)) v env
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


recordAtom :: Env -> [(Value, AccessPath)] -> Maybe Atom
recordAtom env fields =
  ARecord <$> mapM (fieldAtom env) fields


fieldAtom :: Env -> (Value, AccessPath) -> Maybe Atom
fieldAtom env (val, path) =
  applyPath path =<< atomOfValue env val


-- ---------------------------------------------------------------------------
-- Value rewriting under an Env
-- ---------------------------------------------------------------------------

rewriteField :: Env -> (Value, AccessPath) -> (Value, AccessPath)
rewriteField env (val, path) = (rewriteValue env val, path)


rewriteValue :: Env -> Value -> Value
rewriteValue env val@(VVar v) =
  case Map.lookup v env of
    Just (AScalar val') -> val'
    _                   -> val
rewriteValue _ val = val


-- ---------------------------------------------------------------------------
-- Free-variable occurrence checking
-- ---------------------------------------------------------------------------

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
  | v `elem` names  = False
  | v `elem` params = False
  | otherwise       = occursInExp v body


occursInFields :: Variable -> [(Value, AccessPath)] -> Bool
occursInFields v =
  any (\(val, _) -> occursInValue v val)


occursInValue :: Variable -> Value -> Bool
occursInValue v (VVar x) = v == x
occursInValue _ _        = False
