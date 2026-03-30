module Main where

import System.Environment (getArgs)
import Text.Parsec

import Parser
import Resolve
import TopMonad
import CompilePipeline
import QubitHoist (HoistedProgram(..))
import RecordShape (renderModuleRecordShapes)


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
          -- Lambda IR lowering
          let compiledModule = compileModule decls'
          putStrLn "=== Lambda IR ==="
          mapM_ printCompiledItem (compiledItems compiledModule)
          putStrLn $ ""
          putStrLn "=== Module Record Shapes ==="
          mapM_ putStrLn (renderModuleRecordShapes (compiledRecordShapes compiledModule))
  where
    resolution [] = return []
    resolution (d:ds) = do
      sc <- getScope
      (d', sc') <- scopeTop $ resolveDecl sc d
      putScope sc'
      ds' <- resolution ds
      return (d':ds')

    printCompiledItem SkippedDecl =
      return ()
    printCompiledItem (LoweringError err) =
      putStrLn $ "  error: " ++ err
    printCompiledItem (Compiled compiledDecl) = do
      putStrLn $ "  " ++ compiledName compiledDecl ++ " = " ++ show (compiledLambdaIR compiledDecl)
      putStrLn $ ""
      putStrLn "=== CPS IR ==="
      putStrLn $ "  " ++ compiledName compiledDecl ++ " = " ++ show (compiledCPSIR compiledDecl)
      putStrLn $ ""
      putStrLn "=== Recursion Check ==="
      case compiledRecursionResult compiledDecl of
        Left err ->
          putStrLn $ "  error: " ++ err
        Right _ -> do
          putStrLn "  ok (no recursion)"
          putStrLn $ ""
          putStrLn "=== Closure-Converted IR ==="
          maybe (return ()) (\ccExp ->
            putStrLn $ "  " ++ compiledName compiledDecl ++ " = " ++ show ccExp)
            (compiledClosureIR compiledDecl)
          putStrLn $ ""
          putStrLn "=== Defunctionalized IR ==="
          maybe (return ()) (\defuncExp ->
            putStrLn $ "  " ++ compiledName compiledDecl ++ " = " ++ show defuncExp)
            (compiledDefuncIR compiledDecl)
          putStrLn $ ""
          putStrLn "=== Qubit-Hoisted IR ==="
          maybe (return ()) (printHoisted (compiledName compiledDecl)) (compiledHoistedIR compiledDecl)
          putStrLn $ ""
          putStrLn "=== Record-Flattened IR ==="
          maybe (return ()) (\flattened ->
            putStrLn $ "  " ++ compiledName compiledDecl ++ " = " ++ show flattened)
            (compiledFlattenedIR compiledDecl)

    printHoisted name hoisted = do
      putStrLn $ "  qubits = " ++ show (hoistedQubitCount hoisted)
      putStrLn $ "  " ++ name ++ " = " ++ show (hoistedBody hoisted)
