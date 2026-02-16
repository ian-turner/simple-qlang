module Main where

import System.IO
import System.Environment (getArgs)
import Control.Monad.Except
import Control.Exception
import Text.Parsec

import Parser
import Resolve
import TopMonad


parserIO :: Either ParseError a -> IO a
parserIO (Left e) = error $ show e
parserIO (Right a) = return a

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Please provide input filename"
    (srcName : args) -> do
      -- Reading file contents
      fileContents <- readFile srcName
      -- Parsing file into declarations
      (decls, _) <- parserIO $ parseModule srcName fileContents initialParserState
      -- Resolving concrete syntax into abstract syntax
      (decls', st) <- runTop $ resolution decls
      putStrLn $ show decls'
    where
      resolution [] = return []
      resolution (d:ds) = do
        sc <- getScope
        (d', sc') <- scopeTop $ resolveDecl sc d
        putScope sc'
        ds' <- resolution ds
        return (d':ds')
