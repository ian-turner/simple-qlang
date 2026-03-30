module Resolve where

import Nominal
import Nominal.Atom
import Nominal.Atomic
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)

import Utils
import qualified Syntax as A
import qualified ConcreteSyntax as C


-- | Global scope that contains map from strings to names
data Scope = Scope {
  scopeMap :: Map String A.Exp
  }

-- | The empty scope
emptyScope :: Scope
emptyScope = Scope {
--  scopeMap = Map.empty
  scopeMap = Map.fromList [ ("init",   A.Base "init")
                          , ("hgate",  A.Base "hgate")
                          , ("xgate",  A.Base "xgate")
                          , ("zgate",  A.Base "zgate")
                          , ("cnot",   A.Base "cnot")
                          , ("sgate",  A.Base "sgate")
                          , ("tgate",  A.Base "tgate")
                          , ("csgate", A.Base "csgate")
                          , ("ctgate", A.Base "ctgate")
                          , ("cpgate", A.Base "cpgate")
                          , ("meas",   A.Base "meas")
                          ]
  }

-- | Loop up a variable from a global scope using its name
lookupScope :: Scope -> String -> Maybe A.Exp
lookupScope scope x =
  case Map.lookup x (scopeMap scope) of
    Just a  -> Just a
    Nothing -> Nothing

-- | Local scope that contains reference to global scope
data LScope = LScope {
  localScope :: Map String Variable,
  globalScope :: Scope
  }

-- | Constructing a local scope from a global scope
toLScope :: Scope -> LScope
toLScope scope = LScope {
  localScope = Map.empty,
  globalScope = scope
  }

-- | Add a variable name to a local scope
addLScope :: LScope -> String -> Variable -> LScope
addLScope lscope x v =
  let local = localScope lscope
      local' = Map.insert x v local
      lscope' = lscope {localScope = local'}
  in lscope'

-- | Extend the local scope with a list of variables
lscopeVars :: LScope -> [String] -> (LScope -> [Variable] -> a) -> a
lscopeVars lscope ss body =
  freshNames ss $ \ as ->
    let lscope' = foldr (\ (s , a) l -> addLScope l s a) lscope (zip ss as)
    in body lscope' as

-- | Lookup a string from local and global scope
lookupLScope :: LScope -> String -> Maybe A.Exp
lookupLScope lscope x =
  case (Map.lookup x local, lookupScope global x) of
    (Just v, _)        -> Just (A.Var v)
    (Nothing, Just a)  -> Just a
    (Nothing, Nothing) -> Nothing
  where
    local = localScope lscope
    global = globalScope lscope

-- | Information for scope error
data ScopeError
  = NotInScope String
  | MultiDef String
  | LengthMismatch Int Int
  deriving (Show, Eq)

-- | Monad for scope resolution
type Resolve a = Except ScopeError a

-- | A run function for resolving a single exp/decl
runResolve :: Resolve a -> Either ScopeError a
runResolve m = runExcept m

-- | Function for resolving a concrete syntax
-- | expression to an abstract syntax expression
resolve :: LScope -> C.Exp -> Resolve A.Exp

-- | Resolve individual variables
resolve scope (C.Var v) =
  case lookupLScope scope v of
    Nothing -> throwError $ NotInScope v
    Just x  -> return x

-- | Resolve function application
resolve scope (C.App x y) =
  do m <- resolve scope x
     n <- resolve scope y
     return $ A.App m n

-- | Resolve the `unit` expression
resolve scope C.Unit = return A.Unit

-- | Resolve empty `let` bindings
resolve scope (C.Let [] m) = resolve scope m

-- | Resolve single `let` bindings
resolve scope (C.Let ((C.BSingle s n):defs) m) =
  lscopeVars scope [s] $ \ scope' (x:[]) ->
    do n' <- resolve scope n
       m' <- resolve scope' (C.Let defs m)
       return (A.Let n' (x :. m'))

-- | Resolve tuple `let` bindings
resolve scope (C.Let ((C.BTuple vars n):defs) m) =
  lscopeVars scope vars $ \ scope' xs ->
    do n' <- resolve scope n
       m' <- resolve scope' (C.Let defs m)
       return (A.LetTuple n' (xs :. m'))

-- | Resolve tuple expressions
resolve scope (C.Tuple xs) =
  do xs' <- mapM (resolve scope) xs
     return (A.Tuple xs')

-- | Resolve if/then/else expressions
resolve scope (C.IfExp b t f) =
  do b' <- resolve scope b
     t' <- resolve scope t
     f' <- resolve scope f
     return (A.IfExp b' t' f')

-- | Resolve explicit number constants
resolve scope (C.NumInt n) = return (A.NumInt n)
resolve scope (C.NumFloat x) = return (A.NumFloat x)

-- | Resolve boolean literals
resolve scope (C.BoolLit b) = return (A.BoolLit b)

-- | Resolve string literals
resolve scope (C.StringLit s) = return (A.StringLit s)

-- | Resolve infix binary operators: desugar to nested App of Base operator
resolve scope (C.BinOp op e1 e2) = do
  e1' <- resolve scope e1
  e2' <- resolve scope e2
  return $ A.App (A.App (A.Base op) e1') e2'

-- | Resolve dynamic lifting
resolve scope C.Dynlift = return A.Dynlift

-- | Resolve case expressions
resolve scope (C.CaseExp scrut alts) =
  do scrut' <- resolve scope scrut
     alts'  <- mapM (resolveAlt scope) alts
     return (A.Case scrut' alts')

-- | Resolve lambda expressions
resolve scope (C.Lam xs exp) =
  do lscopeVars scope xs $ \ d xs' ->
       do exp' <- resolve d exp
          return (A.Lam (xs' :. exp'))

-- | Resolve a single case alternative, binding pattern variables
resolveAlt :: LScope -> (C.Pat, C.Exp) -> Resolve A.Alt
resolveAlt scope (p, rhs) =
  let vars = patVars p
  in lscopeVars scope vars $ \scope' xs ->
    do let varMap = Map.fromList (zip vars xs)
       let p'    = resolvePat varMap p
       rhs'     <- resolve scope' rhs
       return $ A.Alt (xs :. (p', rhs'))

-- | Extract variable names from a flat concrete pattern
patVars :: C.Pat -> [String]
patVars (C.PVar x)       = [x]
patVars C.PWild          = []
patVars (C.PCon _ args)  = [x | C.FArg x <- args]
patVars (C.PTuple args)  = [x | C.FArg x <- args]
patVars C.PUnit          = []

-- | Translate a concrete pattern to abstract, using a variable map
resolvePat :: Map String Variable -> C.Pat -> A.Pat
resolvePat m (C.PVar x)       = A.PVar (m Map.! x)
resolvePat _ C.PWild          = A.PWild
resolvePat m (C.PCon n args)  = A.PCon n (map (resolveField m) args)
resolvePat m (C.PTuple args)  = A.PTuple (map (resolveField m) args)
resolvePat _ C.PUnit          = A.PUnit

resolveField :: Map String Variable -> C.FlatArg -> A.PatField
resolveField m (C.FArg x) = A.PFVar (m Map.! x)
resolveField _ C.FWild    = A.PFWild

-- | Add a constant to the scope
addConst :: String -> (String -> A.Exp) -> Scope -> Resolve Scope
addConst x f scope =
  case lookupScope scope x of
    Just x' -> throwError $ MultiDef x
    Nothing ->
      let newMap = Map.insert x (f x) $ scopeMap scope
          scope' = scope { scopeMap = newMap }
      in (return scope')

-- | Function for resolving a concrete syntax
-- | declaration to an abstract syntax declaration
resolveDecl :: Scope -> C.Decl -> Resolve (A.Decl, Scope)

-- | Resolve global variable definitions
resolveDecl scope (C.VarDef name exp) =
  do scope' <- addConst name A.Const scope
     let lscope = toLScope scope'
     exp' <- resolve lscope exp
     return (A.Def name exp', scope')

-- | Resolve function definitions
resolveDecl scope (C.FunDef name args def) =
  do scope' <- addConst name A.Const scope
     let lscope = toLScope scope'
     lscopeVars lscope args $ \ d xs ->
       do def' <- resolve d def
          return (A.Def name (A.Lam (xs :. def')), scope')

-- | Resolve type annotations (no scope changes, types are not yet checked)
resolveDecl scope (C.TypeSig name typeExp) =
  return (A.TypeSig name (resolveType typeExp), scope)

-- | Resolve data type declarations, adding each constructor to scope
resolveDecl scope (C.DataDecl name vars condecls) = do
  scope' <- foldM addConToScope scope condecls
  return (A.DataDecl name vars (map resolveConDecl condecls), scope')
  where
    addConToScope sc (C.ConDecl conName _) =
      addConst conName A.Con sc
    resolveConDecl (C.ConDecl conName fields) =
      A.ConDecl conName (map resolveType fields)

-- | Translate a concrete type expression to abstract syntax
resolveType :: C.TypeExp -> A.TypeExp
resolveType (C.TyVar s)      = A.TyVar s
resolveType (C.TyCon s)      = A.TyCon s
resolveType (C.TyApp t1 t2)  = A.TyApp (resolveType t1) (resolveType t2)
resolveType (C.TyFun t1 t2)  = A.TyFun (resolveType t1) (resolveType t2)
resolveType (C.TyTuple ts)   = A.TyTuple (map resolveType ts)
