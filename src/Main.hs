module Main where

import System.IO
import System.Environment (getArgs)
import Control.Monad.Except
import Control.Exception
import Text.Parsec

import Parser
import Resolve
import TopMonad
import Lower (lowerDecl, runLower)
import ToCPS (toCPSDecl)
import RecElim (elimRecursion)
import ClosureConv (closureConvert)
import Defunc (defunctionalize)
import QubitHoist (HoistedProgram(..), hoistQubits)


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
      (decls, _) <- parserIO $ parseModule srcName fileContents initialParserState
      -- Scope resolution
      (result, _) <- runTop $ resolution decls
      case result of
        Left err    -> error $ show err
        Right decls' -> do
          -- Lambda IR lowering
          putStrLn "=== Lambda IR ==="
          mapM_ printLowered decls'
  where
    resolution [] = return []
    resolution (d:ds) = do
      sc <- getScope
      (d', sc') <- scopeTop $ resolveDecl sc d
      putScope sc'
      ds' <- resolution ds
      return (d':ds')

    printLowered d =
      case runLower (lowerDecl d) of
        Left err        -> putStrLn $ "  error: " ++ err
        Right Nothing   -> return ()
        Right (Just (name, lexp)) -> do
          putStrLn $ "  " ++ name ++ " = " ++ show lexp
          putStrLn $ ""
          putStrLn "=== CPS IR ==="
          let cpsExp = toCPSDecl name lexp
          putStrLn $ "  " ++ name ++ " = " ++ show cpsExp
          putStrLn $ ""
          putStrLn "=== Recursion Check ==="
          case elimRecursion cpsExp of
            Left err     -> putStrLn $ "  error: " ++ err
            Right cpsExp' -> do
              putStrLn "  ok (no recursion)"
              putStrLn $ ""
              putStrLn "=== Closure-Converted IR ==="
              let ccExp = closureConvert cpsExp'
              putStrLn $ "  " ++ name ++ " = " ++ show ccExp
              putStrLn $ ""
              putStrLn "=== Defunctionalized IR ==="
              let defuncExp = defunctionalize ccExp
              putStrLn $ "  " ++ name ++ " = " ++ show defuncExp
              putStrLn $ ""
              putStrLn "=== Qubit-Hoisted IR ==="
              let hoisted = hoistQubits defuncExp
              putStrLn $ "  qubits = " ++ show (hoistedQubitCount hoisted)
              putStrLn $ "  " ++ name ++ " = " ++ show (hoistedBody hoisted)
