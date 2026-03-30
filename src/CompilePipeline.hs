module CompilePipeline
  ( CompiledModule(..)
  , CompiledItem(..)
  , CompiledDecl(..)
  , compileModule
  ) where

import Syntax (Decl)
import LambdaIR (LExp)
import CPSExp (CExp)
import Lower (lowerDecl, runLower)
import ToCPS (toCPSDecl)
import RecElim (elimRecursion)
import ClosureConv (closureConvert)
import Defunc (defunctionalize)
import QubitHoist (HoistedProgram(..), hoistQubits)
import RecordFlatten (flattenRecords)
import RecordShape (ModuleRecordShapes, analyzeModuleRecordShapes)


data CompiledModule = CompiledModule
  { compiledItems       :: [CompiledItem]
  , compiledEntryPoints :: [String]
  , compiledRecordShapes :: ModuleRecordShapes
  }


data CompiledItem
  = SkippedDecl
  | LoweringError String
  | Compiled CompiledDecl


data CompiledDecl = CompiledDecl
  { compiledName            :: String
  , compiledLambdaIR        :: LExp
  , compiledCPSIR           :: CExp
  , compiledRecursionResult :: Either String CExp
  , compiledClosureIR       :: Maybe CExp
  , compiledDefuncIR        :: Maybe CExp
  , compiledHoistedIR       :: Maybe HoistedProgram
  , compiledFlattenedIR     :: Maybe CExp
  }


compileModule :: [Decl] -> CompiledModule
compileModule decls =
  let items = map compileDecl decls
  in CompiledModule
       { compiledItems = items
       , compiledEntryPoints = findEntryPoints items
       , compiledRecordShapes = analyzeModuleRecordShapes (shapeInputs items)
       }


compileDecl :: Decl -> CompiledItem
compileDecl decl =
  case runLower (lowerDecl decl) of
    Left err -> LoweringError err
    Right Nothing -> SkippedDecl
    Right (Just (name, lexp)) ->
      let cpsExp = toCPSDecl name lexp
      in case elimRecursion cpsExp of
           Left err ->
             Compiled CompiledDecl
               { compiledName = name
               , compiledLambdaIR = lexp
               , compiledCPSIR = cpsExp
               , compiledRecursionResult = Left err
               , compiledClosureIR = Nothing
               , compiledDefuncIR = Nothing
               , compiledHoistedIR = Nothing
               , compiledFlattenedIR = Nothing
               }
           Right cpsExp' ->
             let ccExp = closureConvert cpsExp'
                 defuncExp = defunctionalize ccExp
                 hoisted = hoistQubits defuncExp
             in Compiled CompiledDecl
                  { compiledName = name
                  , compiledLambdaIR = lexp
                  , compiledCPSIR = cpsExp
                  , compiledRecursionResult = Right cpsExp'
                  , compiledClosureIR = Just ccExp
                  , compiledDefuncIR = Just defuncExp
                  , compiledHoistedIR = Just hoisted
                  , compiledFlattenedIR = Just (flattenRecords (hoistedBody hoisted))
                  }


findEntryPoints :: [CompiledItem] -> [String]
findEntryPoints items =
  [ compiledName decl
  | Compiled decl <- items
  , compiledName decl == "output"
  ]


shapeInputs :: [CompiledItem] -> [(String, Either String CExp)]
shapeInputs items =
  [ (compiledName decl, compiledRecursionResult decl)
  | Compiled decl <- items
  ]
