{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TopMonad where

import Nominal
import Control.Monad.Except
import Control.Monad.Identity
import Control.Exception
import Control.Monad.State
import Control.Monad.Catch hiding (catch)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Text.Parsec hiding (count)

import Syntax as A
import ConcreteSyntax as C
import Parser
import Resolve
import Utils


-- | Top-level errors
data Error
  = ParseErr ParseError
  | ScopeErr ScopeError
  deriving (Show)

-- | Top-level monad - wraps on top of IO monad and maintains state
newtype Top a = T { runT :: ExceptT Error (StateT TopState IO) a }
  deriving (Monad, Applicative, Functor, MonadState TopState,
            MonadError Error, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | TopState contains the compiler state and file name
data TopState = TopState {
  compilerState :: CompilerState
  }

-- | Initial top-level state
initTopState = TopState {
  compilerState = emptyState
  }

-- | Project Top monad into IO monad
runTop :: Top a -> IO (Either Error a, TopState)
runTop body = runStateT (runExceptT (runT body)) initTopState

-- | Compiler's state
data CompilerState = CompilerState {
  scope :: Scope
  }

-- | Update the compiler's state
putCompilerState :: CompilerState -> Top ()
putCompilerState cs = do
  s <- get
  put $ s { compilerState = cs }

-- | The empty compiler state
emptyState = CompilerState {
  scope = emptyScope
  }

-- | Get current compiler state
getCompilerState :: Top CompilerState
getCompilerState = do
  s <- get
  return $ compilerState s

-- | Get current scope
getScope :: Top Scope
getScope = do
  s <- getCompilerState
  return $ scope s

-- | Update scope
putScope :: Scope -> Top ()
putScope scope = do
  s <- getCompilerState
  let s' = s { scope = scope }
  putCompilerState s'

-- | Lift the Resolve monad into the Top monad
scopeTop :: Resolve a -> Top a
scopeTop x =
  case runResolve x of
    Left e  -> throwError $ ScopeErr e
    Right a -> return a

-- | Resolve an expression at the top level
topResolve :: C.Exp -> Top A.Exp
topResolve t = do
  scope <- getScope
  scopeTop $ resolve (toLScope scope) t

-- | Lifting a parsing result to Top monad
parserTop :: Either ParseError a -> Top a
parserTop (Left e) = throwError (ParseErr e)
parserTop (Right a) = return a
