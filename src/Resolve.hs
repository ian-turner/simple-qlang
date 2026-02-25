module Resolve where

import Nominal
import Nominal.Atom
import Nominal.Atomic
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
  scopeMap = Map.fromList [ ("init", A.Base "init")
                          , ("hgate", A.Base "hgate")
                          , ("xgate", A.Base "hgate")
                          , ("zgate", A.Base "hgate")
                          , ("cnot", A.Base "cnot")
                          , ("meas", A.Base "meas")
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
type Resolve a = ExceptT ScopeError (StateT () Identity) a

-- | A run function for resolving a single exp/decl
runResolve :: Resolve a -> Either ScopeError a
runResolve m = runIdentity $ evalStateT (runExceptT m) ()

-- | Function for resolving a concrete syntax
-- | expression to an abstract syntax expression
resolve :: LScope -> C.Exp -> Resolve A.Exp
resolve scope (C.Var v) =
  case lookupLScope scope v of
    Nothing -> throwError $ NotInScope v
    Just x  -> return x

resolve scope (C.App x y) =
  do m <- resolve scope x
     n <- resolve scope y
     return $ A.App m n

resolve scope C.Unit = return A.Unit

resolve scope (C.Let [] m) = resolve scope m

resolve scope (C.Let ((C.BSingle s n):defs) m) =
  lscopeVars scope [s] $ \ scope' (x:[]) ->
    do n' <- resolve scope n
       m' <- resolve scope' (C.Let defs m)
       return (A.Let n' (x :. m'))

resolve scope (C.Let ((C.BTuple vars n):defs) m) =
  lscopeVars scope vars $ \ scope' xs ->
    do n' <- resolve scope n
       m' <- resolve scope' (C.Let defs m)
       return (A.LetTuple n' (xs :. m'))

resolve scope (C.Tuple xs) =
  do xs' <- mapM (resolve scope) xs
     return (A.Tuple xs')

resolve scope (C.IfExp b t f) =
  do b' <- resolve scope b
     t' <- resolve scope t
     f' <- resolve scope f
     return (A.IfExp b' t' f')

resolve scope (C.NumInt n) = return (A.NumInt n)

resolve scope (C.NumFloat x) = return (A.NumFloat x)

resolve scope C.Dynlift = return A.Dynlift

resolve scope (C.Lam xs exp) =
  do lscopeVars scope xs $ \ d xs' ->
       do exp' <- resolve d exp
          return (A.Lam (xs' :. exp'))

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
resolveDecl scope (C.VarDef name exp) =
  do scope' <- addConst name A.Const scope
     let lscope = toLScope scope'
     exp' <- resolve lscope exp
     return (A.Def name exp', scope')

resolveDecl scope (C.FunDef name args def) =
  do scope' <- addConst name A.Const scope
     let lscope = toLScope scope'
     lscopeVars lscope args $ \ d xs ->
       do def' <- resolve d def
          return (A.Def name (A.Lam (xs :. def')), scope')
