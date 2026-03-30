module Main where

import System.Environment (getArgs)
import Text.Parsec

import Parser
import Resolve
import TopMonad
import CompilePipeline
import OpenQASM (emitOpenQASM)


parserIO :: Either ParseError a -> IO a
parserIO (Left e) = error $ show e
parserIO (Right a) = return a

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Please provide input filename"
    (srcName : _) -> do
      -- Parsing
      fileContents <- readFile srcName
      (parsedDecls, _) <- parserIO $ parseModule srcName fileContents initialParserState
      -- Scope resolution
      (result, _) <- runTop $ resolution parsedDecls
      case result of
        Left err    -> error $ show err
        Right decls' -> do
          let compiledModule = compileModule decls'
          case emitOpenQASM compiledModule of
            Left err -> error err
            Right qasm -> putStrLn qasm
  where
    resolution [] = return []
    resolution (d:ds) = do
      sc <- getScope
      (d', sc') <- scopeTop $ resolveDecl sc d
      putScope sc'
      ds' <- resolution ds
      return (d':ds')
