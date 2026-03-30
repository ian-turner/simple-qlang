{-# LANGUAGE TupleSections #-}
module Lower
  ( lowerDecl
  , lowerExp
  , runLower
  ) where

import Control.Monad.Except
import Data.Maybe (listToMaybe)

import Nominal

import Utils
import Syntax
import LambdaIR


-- | The lowering monad: error handling only.
--   Fresh variable creation uses CPS-style 'freshNames' from Utils.
type Lower a = Except String a

runLower :: Lower a -> Either String a
runLower = runExcept

-- ---------------------------------------------------------------------------
-- Primitive name → PrimOp

baseToPrimOp :: String -> Maybe PrimOp
baseToPrimOp "init"  = Just PInit
baseToPrimOp "meas"  = Just PMeas
baseToPrimOp "hgate" = Just PHGate
baseToPrimOp "xgate" = Just PXGate
baseToPrimOp "zgate" = Just PZGate
baseToPrimOp "cnot"  = Just PCNot
baseToPrimOp "+"     = Just PAdd
baseToPrimOp "-"     = Just PSub
baseToPrimOp "*"     = Just PMul
baseToPrimOp "/"     = Just PDiv
baseToPrimOp "=="    = Just PEq
baseToPrimOp "<"     = Just PLt
baseToPrimOp ">"     = Just PGt
baseToPrimOp "<="    = Just PLe
baseToPrimOp ">="    = Just PGe
baseToPrimOp "&&"    = Just PAnd
baseToPrimOp "||"    = Just POr
baseToPrimOp "not"   = Just PNot
baseToPrimOp _       = Nothing

-- ---------------------------------------------------------------------------
-- Application-chain collectors

-- | If the head of a curried application chain is a constructor, return
--   (conName, [arg1, arg2, ...]).
collectConArgs :: Exp -> Maybe (String, [Exp])
collectConArgs (Con s)   = Just (s, [])
collectConArgs (App f e) = fmap (\(s, args) -> (s, args ++ [e])) (collectConArgs f)
collectConArgs _         = Nothing

-- | If the head of a curried application chain is a primitive, return
--   (primOp, [arg1, arg2, ...]).
collectPrimArgs :: Exp -> Maybe (PrimOp, [Exp])
collectPrimArgs (Base s)   = fmap (, []) (baseToPrimOp s)
collectPrimArgs (App f e)  = fmap (\(op, args) -> (op, args ++ [e])) (collectPrimArgs f)
collectPrimArgs _          = Nothing

-- ---------------------------------------------------------------------------
-- Let-binding chaining

-- | Encode a list of bindings as nested LApp (LLam v body) e.
--   wrapLets [(v1,e1),(v2,e2)] body
--     = LApp (LLam v1 (LApp (LLam v2 body) e2)) e1
wrapLets :: [(Variable, LExp)] -> LExp -> LExp
wrapLets []            body = body
wrapLets ((v, e):rest) body = LApp (LLam v (wrapLets rest body)) e

-- ---------------------------------------------------------------------------
-- Expression lowering

-- | Lower a resolved Syntax.Exp to LambdaIR.LExp.
lowerExp :: Exp -> Lower LExp

-- Literals
lowerExp Unit          = return (LLit LUnit)
lowerExp (NumInt n)    = return (LLit (LInt n))
lowerExp (NumFloat f)  = return (LLit (LFloat f))
lowerExp (BoolLit b)   = return (LLit (LBool b))
lowerExp (StringLit s) = return (LLit (LString s))

-- Variables and names
lowerExp (Var v)   = return (LVar v)
lowerExp (Const s) = return (LTopVar s)

-- Built-in primitive: standalone → LPrim op [] (first-class function value).
lowerExp (Base s) =
  return $ case baseToPrimOp s of
    Just op -> LPrim op []
    Nothing -> LTopVar s

-- Constructor: standalone constant constructor → LCon s LUnit.
-- Value-carrying constructors used as expressions always appear as App (Con s) e
-- and are handled in lowerApp via collectConArgs.
lowerExp (Con s) = return (LCon s (LLit LUnit))

-- Tuple
lowerExp (Tuple es) = LTuple <$> mapM lowerExp es

-- Application — see lowerApp
lowerExp (App f e) = lowerApp f e

-- Lambda: open binding, nest into single-arg LLams
lowerExp (Lam bind) =
  case bind of
    (vs :. body) -> do
      body' <- lowerExp body
      return (foldr LLam body' vs)

-- Let x = e in body  →  LApp (LLam x body') e'
lowerExp (Let e bind) =
  case bind of
    (v :. body) -> do
      e'    <- lowerExp e
      body' <- lowerExp body
      return (LApp (LLam v body') e')

-- let (v1,..,vn) = e in body  →  bind fresh t, select each field
lowerExp (LetTuple e bind) =
  case bind of
    (vs :. body) -> do
      e'    <- lowerExp e
      body' <- lowerExp body
      freshNames ["t"] $ \[t] ->
        let selects = [(v, LSelect i (LVar t)) | (i, v) <- zip [0..] vs]
        in return (LApp (LLam t (wrapLets selects body')) e')

-- if b then t else f  →  LSwitch on True/False constructors
lowerExp (IfExp b t f) = do
  b' <- lowerExp b
  t' <- lowerExp t
  f' <- lowerExp f
  return (LSwitch b' [(CACon "True", t'), (CACon "False", f')] Nothing)

-- case scrut of alts  →  bind scrutinee to fresh t, build LSwitch
lowerExp (Case scrut alts) = do
  scrut' <- lowerExp scrut
  freshNames ["t"] $ \[t] -> do
    arms <- mapM (lowerAlt t) alts
    let taggedArms = [(tag, body) | (Just tag, body) <- arms]
        defaultArm = listToMaybe [body | (Nothing, body) <- arms]
    return (LApp (LLam t (LSwitch (LVar t) taggedArms defaultArm)) scrut')

lowerExp Dynlift = throwError "Dynlift not supported in lambda lowering"

-- ---------------------------------------------------------------------------
-- Application

-- | Lower an application, collecting primitive/constructor argument chains.
lowerApp :: Exp -> Exp -> Lower LExp
lowerApp f e =
  case collectConArgs (App f e) of
    -- Constructor applied to ≥1 argument: collect all args, wrap in LCon
    Just (s, args@(_:_)) -> do
      args' <- mapM lowerExp args
      let payload = case args' of
                      [a] -> a
                      _   -> LTuple args'
      return (LCon s payload)
    Nothing ->
      case collectPrimArgs (App f e) of
        -- Primitive applied to ≥1 argument: collect all args into LPrim
        Just (op, args@(_:_)) -> do
          args' <- mapM lowerExp args
          return (LPrim op args')
        -- General application
        _ -> do
          f' <- lowerExp f
          e' <- lowerExp e
          return (LApp f' e')

-- ---------------------------------------------------------------------------
-- Pattern flattening

-- | Given the scrutinee variable t and a pattern, produce:
--   * Maybe ConAlt — the constructor tag (Nothing for irrefutable patterns)
--   * [(Variable, LExp)] — variable bindings to introduce via wrapLets
flattenPat :: Variable -> Pat -> Lower (Maybe ConAlt, [(Variable, LExp)])

flattenPat t (PVar v) = return (Nothing, [(v, LVar t)])
flattenPat _ PWild    = return (Nothing, [])
flattenPat _ PUnit    = return (Just CAUnit, [])

flattenPat t (PTuple fields) =
  return ( Nothing
         , [(v, LSelect i (LVar t)) | (i, PFVar v) <- zip [0..] fields]
         )

-- Constant constructor (no fields): just test the tag, no payload binding
flattenPat _ (PCon name []) =
  return (Just (CACon name), [])

-- Value-carrying constructor: decon to fresh p, then select fields from p
flattenPat t (PCon name fields) =
  freshNames ["p"] $ \[p] ->
    return ( Just (CACon name)
           , (p, LDecon name (LVar t))
             : [(v, LSelect i (LVar p)) | (i, PFVar v) <- zip [0..] fields]
           )

-- ---------------------------------------------------------------------------
-- Case alternative lowering

-- | Lower a single case alternative.
--   Opens the Bind to get (pat, rhs), flattens the pattern, wraps rhs in bindings.
lowerAlt :: Variable -> Alt -> Lower (Maybe ConAlt, LExp)
lowerAlt t (Alt bind) =
  case bind of
    (_vs :. (pat, rhs)) -> do
      (mTag, bindings) <- flattenPat t pat
      rhs'             <- lowerExp rhs
      return (mTag, wrapLets bindings rhs')

-- ---------------------------------------------------------------------------
-- Declaration lowering

-- | Lower a top-level declaration.
--   Returns Just (name, lexp) for definitions, Nothing for type/data decls.
lowerDecl :: Decl -> Lower (Maybe (String, LExp))
lowerDecl (Def name e)       = Just . (name,) <$> lowerExp e
lowerDecl (TypeSig _ _)      = return Nothing
lowerDecl (DataDecl _ _ _)   = return Nothing
