module Main where

import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Parsec

import Parser
import Resolve
import TopMonad
import CompilePipeline
import OpenQASM (emitOpenQASM)
import RecordShape (renderModuleRecordShapes)
import GateDef (renderModuleCallableKinds, renderCallableKind)


parserIO :: Either ParseError a -> IO a
parserIO (Left e) = error $ show e
parserIO (Right a) = return a

main :: IO ()
main = do
  args <- getArgs
  let (debugMode, positionalArgs) = parseArgs args
  case positionalArgs of
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
          if debugMode
            then putStr (renderDebugReport srcName parsedDecls decls' compiledModule)
            else pure ()
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


parseArgs :: [String] -> (Bool, [String])
parseArgs = foldr step (False, [])
  where
    step "--debug" (_, rest) = (True, rest)
    step arg (debugMode, rest) = (debugMode, arg : rest)


renderDebugReport :: (Show a, Show b) => FilePath -> [a] -> [b] -> CompiledModule -> String
renderDebugReport srcName parsedDecls resolvedDecls compiledModule =
  unlines $
    [ section ("Input File: " ++ srcName)
    , section "Parsed Declarations"
    ]
    ++ map show parsedDecls
    ++ [ section "Resolved Declarations" ]
    ++ map show resolvedDecls
    ++ [ section "Module Record Shapes" ]
    ++ renderModuleRecordShapes (compiledRecordShapes compiledModule)
    ++ [ section "Module Callable Kinds" ]
    ++ renderModuleCallableKinds (compiledCallableKinds compiledModule)
    ++ [ section ("Entry Points: " ++ renderList (compiledEntryPoints compiledModule))
       , section "Per-Declaration Pipeline"
       ]
    ++ concatMap renderCompiledItem (compiledItems compiledModule)


renderCompiledItem :: CompiledItem -> [String]
renderCompiledItem SkippedDecl =
  [ section "Skipped Declaration"
  , "No Lambda IR generated for this declaration."
  ]
renderCompiledItem (LoweringError err) =
  [ section "Lowering Error"
  , err
  ]
renderCompiledItem (Compiled compiledDecl) =
  [ section ("Declaration: " ++ compiledName compiledDecl)
  , "Lambda IR:"
  , show (compiledLambdaIR compiledDecl)
  , "Initial CPS IR:"
  , show (compiledCPSIR compiledDecl)
  , "Recursion Check:"
  ]
  ++ either
       (\err -> ["error: " ++ err])
       (\expr -> ["ok", "Recursion-Normalized CPS IR:", show expr])
       (compiledRecursionResult compiledDecl)
  ++ renderMaybe "Interface-Flattened CPS IR:" show (compiledInterfaceIR compiledDecl)
  ++ renderMaybe "Callable Kind:" renderCallableKind (compiledCallableKind compiledDecl)
  ++ renderMaybe "Closure-Converted CPS IR:" show (compiledClosureIR compiledDecl)
  ++ renderMaybe "Defunctionalized CPS IR:" show (compiledDefuncIR compiledDecl)
  ++ renderMaybe "Qubit-Hoisted IR:" show (compiledHoistedIR compiledDecl)
  ++ renderMaybe "Record-Flattened IR:" show (compiledFlattenedIR compiledDecl)


renderMaybe :: String -> (a -> String) -> Maybe a -> [String]
renderMaybe _ _ Nothing = []
renderMaybe heading renderValue (Just value) =
  [ heading
  , renderValue value
  ]


renderList :: [String] -> String
renderList [] = "(none)"
renderList xs = intercalate ", " xs


section :: String -> String
section title =
  "\n== " ++ title ++ " =="
