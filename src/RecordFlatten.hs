module RecordFlatten
  ( flattenRecords
  ) where

import qualified Data.Map.Strict as Map

import CPSExp
import Utils (Variable)


data Atom
  = AScalar Value
  | ARecord [Atom]

type Env = Map.Map Variable Atom


flattenRecords :: CExp -> CExp
flattenRecords = flattenExp Map.empty


flattenExp :: Env -> CExp -> CExp
flattenExp env (CRecord fields v body) =
  let fields' = map (rewriteField env) fields
      env' = Map.delete v env
      body' =
        case recordAtom env fields' of
          Just atom -> flattenExp (Map.insert v atom env') body
          Nothing   -> flattenExp env' body
  in if occursInExp v body'
       then CRecord fields' v body'
       else body'

flattenExp env (CSelect i val v body) =
  let val' = rewriteValue env val
      env' = Map.delete v env
  in case selectAtom env val' i of
       Just atom ->
         let body' = flattenExp (Map.insert v atom env') body
         in if occursInExp v body'
              then CSelect i val' v body'
              else body'
       Nothing ->
         CSelect i val' v (flattenExp env' body)

flattenExp env (COffset off val v body) =
  let val' = rewriteValue env val
      env' = Map.delete v env
  in case offsetAtom env val' off of
       Just atom ->
         let body' = flattenExp (Map.insert v atom env') body
         in if occursInExp v body'
              then COffset off val' v body'
              else body'
       Nothing ->
         COffset off val' v (flattenExp env' body)

flattenExp env (CApp fn args) =
  CApp (rewriteValue env fn) (map (rewriteValue env) args)

flattenExp env (CFix defs body) =
  let names = map (\(f, _, _) -> f) defs
      env' = foldr Map.delete env names
      defs' = map (flattenDef env) defs
  in CFix defs' (flattenExp env' body)

flattenExp env (CSwitch val arms) =
  CSwitch (rewriteValue env val) (map (flattenExp env) arms)

flattenExp env (CPrimOp op args results conts) =
  let env' = foldr Map.delete env results
  in CPrimOp op
       (map (rewriteValue env) args)
       results
       (map (flattenExp env') conts)


flattenDef :: Env -> (Variable, [Variable], CExp) -> (Variable, [Variable], CExp)
flattenDef env (f, params, body) =
  let env' = foldr Map.delete (Map.delete f env) params
  in (f, params, flattenExp env' body)


recordAtom :: Env -> [(Value, AccessPath)] -> Maybe Atom
recordAtom env fields =
  ARecord <$> mapM (fieldAtom env) fields


fieldAtom :: Env -> (Value, AccessPath) -> Maybe Atom
fieldAtom env (val, path) =
  applyPath path =<< atomOfValue env val


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


rewriteField :: Env -> (Value, AccessPath) -> (Value, AccessPath)
rewriteField env (val, path) = (rewriteValue env val, path)


rewriteValue :: Env -> Value -> Value
rewriteValue env val@(VVar v) =
  case Map.lookup v env of
    Just (AScalar val') -> val'
    _                   -> val
rewriteValue _ val = val


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
