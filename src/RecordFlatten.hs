module RecordFlatten
  ( flattenRecords
  ) where

import qualified Data.Map.Strict as Map

import CPSExp
import CPSAtom
import Utils (Variable)


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

flattenExp env (CFor idx lo hi body cont) =
  let lo'   = rewriteValue env lo
      hi'   = rewriteValue env hi
      env'  = Map.delete idx env
      body' = flattenExp env' body
      cont' = flattenExp env cont
  in CFor idx lo' hi' body' cont'


flattenDef :: Env -> (Variable, [Variable], CExp) -> (Variable, [Variable], CExp)
flattenDef env (f, params, body) =
  let env' = foldr Map.delete (Map.delete f env) params
  in (f, params, flattenExp env' body)
