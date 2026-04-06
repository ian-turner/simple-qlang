module QubitHoist
  ( HoistedProgram(..)
  , hoistQubits
  ) where

import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

import CPSExp
import LambdaIR (PrimOp(..))
import Utils (Variable)


data HoistedProgram = HoistedProgram
  { hoistedQubitCount :: Int
  , hoistedBody       :: CExp
  }
  deriving (Show)


type Subst = Map.Map Variable Value
type HoistM = State Int


hoistQubits :: CExp -> HoistedProgram
hoistQubits expr =
  let (expr', count) = runState (hoistExp Map.empty expr) 0
  in HoistedProgram
       { hoistedQubitCount = count
       , hoistedBody = expr'
       }


hoistExp :: Subst -> CExp -> HoistM CExp
hoistExp env (CRecord fields v body) = do
  body' <- hoistExp (Map.delete v env) body
  pure $ CRecord (map (rewriteField env) fields) v body'

hoistExp env (CSelect i val v body) = do
  body' <- hoistExp (Map.delete v env) body
  pure $ CSelect i (rewriteValue env val) v body'

hoistExp env (CApp fn args) =
  pure $ CApp (rewriteValue env fn) (map (rewriteValue env) args)

hoistExp env (CFix defs body) = do
  let names = map (\(f, _, _) -> f) defs
      bodyEnv = foldr Map.delete env names
  defs' <- mapM (hoistDef env) defs
  body' <- hoistExp bodyEnv body
  pure $ CFix defs' body'

hoistExp env (CSwitch val arms) = do
  arms' <- mapM (hoistExp env) arms
  pure $ CSwitch (rewriteValue env val) arms'

hoistExp env (CPrimOp PInit [VUnit] [q] [cont]) = do
  slot <- get
  put (slot + 1)
  hoistExp (Map.insert q (VQubit slot) env) cont

hoistExp env (CPrimOp op args results conts) = do
  let env' = foldr Map.delete env results
  conts' <- mapM (hoistExp env') conts
  pure $ CPrimOp op (map (rewriteValue env) args) results conts'

hoistExp env (COffset off val v body) = do
  body' <- hoistExp (Map.delete v env) body
  pure $ COffset off (rewriteValue env val) v body'

hoistExp env (CFor idx lo hi body cont) = do
  let env' = Map.delete idx env
  body' <- hoistExp env' body
  cont' <- hoistExp env cont
  pure $ CFor idx (rewriteValue env lo) (rewriteValue env hi) body' cont'


hoistDef :: Subst -> (Variable, [Variable], CExp) -> HoistM (Variable, [Variable], CExp)
hoistDef env (f, params, body) = do
  let env' = foldr Map.delete (Map.delete f env) params
  body' <- hoistExp env' body
  pure (f, params, body')


rewriteField :: Subst -> (Value, AccessPath) -> (Value, AccessPath)
rewriteField env (val, path) = (rewriteValue env val, path)


rewriteValue :: Subst -> Value -> Value
rewriteValue env val@(VVar v) =
  Map.findWithDefault val v env
rewriteValue env (VQubitArr base idx) = VQubitArr base (rewriteValue env idx)
rewriteValue _ val = val
