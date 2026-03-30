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
import ModuleRecordFlatten (flattenModuleRecordInterfaces)
import GateDef (CallableKind, ModuleCallableKinds, analyzeModuleCallableKinds, lookupTopLevelCallableKind)
import ClosureConv (closureConvert)
import Defunc (defunctionalize)
import QubitHoist (HoistedProgram(..), hoistQubits)
import RecordFlatten (flattenRecords)
import RecordShape (ModuleRecordShapes, analyzeModuleRecordShapes)


data CompiledModule = CompiledModule
  { compiledItems       :: [CompiledItem]
  , compiledEntryPoints :: [String]
  , compiledRecordShapes :: ModuleRecordShapes
  , compiledCallableKinds :: ModuleCallableKinds
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
  , compiledInterfaceIR     :: Maybe CExp
  , compiledCallableKind    :: Maybe CallableKind
  , compiledClosureIR       :: Maybe CExp
  , compiledDefuncIR        :: Maybe CExp
  , compiledHoistedIR       :: Maybe HoistedProgram
  , compiledFlattenedIR     :: Maybe CExp
  }


compileModule :: [Decl] -> CompiledModule
compileModule decls =
  let initialItems = map compileDecl decls
      shapes = analyzeModuleRecordShapes (shapeInputs initialItems)
      interfacedItems = map (prepareDecl shapes) initialItems
      callableKinds = analyzeModuleCallableKinds (classificationInputs interfacedItems)
      items = map (finalizeDecl callableKinds) interfacedItems
  in CompiledModule
       { compiledItems = items
       , compiledEntryPoints = findEntryPoints items
       , compiledRecordShapes = shapes
       , compiledCallableKinds = callableKinds
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
               , compiledInterfaceIR = Nothing
               , compiledCallableKind = Nothing
               , compiledClosureIR = Nothing
               , compiledDefuncIR = Nothing
               , compiledHoistedIR = Nothing
               , compiledFlattenedIR = Nothing
               }
           Right cpsExp' ->
             Compiled CompiledDecl
                 { compiledName = name
                  , compiledLambdaIR = lexp
                  , compiledCPSIR = cpsExp
                  , compiledRecursionResult = Right cpsExp'
                  , compiledInterfaceIR = Nothing
                  , compiledCallableKind = Nothing
                  , compiledClosureIR = Nothing
                  , compiledDefuncIR = Nothing
                  , compiledHoistedIR = Nothing
                  , compiledFlattenedIR = Nothing
                  }


prepareDecl :: ModuleRecordShapes -> CompiledItem -> CompiledItem
prepareDecl _ SkippedDecl = SkippedDecl
prepareDecl _ (LoweringError err) = LoweringError err
prepareDecl shapes (Compiled compiledDecl) =
  case compiledRecursionResult compiledDecl of
    Left _ ->
      Compiled compiledDecl
    Right cpsExp ->
      let interfaceExp = flattenModuleRecordInterfaces shapes (compiledName compiledDecl) cpsExp
      in Compiled compiledDecl
           { compiledInterfaceIR = Just interfaceExp
           }


finalizeDecl :: ModuleCallableKinds -> CompiledItem -> CompiledItem
finalizeDecl _ SkippedDecl = SkippedDecl
finalizeDecl _ (LoweringError err) = LoweringError err
finalizeDecl callableKinds (Compiled compiledDecl) =
  case compiledInterfaceIR compiledDecl of
    Nothing ->
      Compiled compiledDecl
    Just interfaceExp ->
      let ccExp = closureConvert interfaceExp
          defuncExp = defunctionalize ccExp
          hoisted = hoistQubits defuncExp
      in Compiled compiledDecl
           { compiledCallableKind = lookupTopLevelCallableKind callableKinds (compiledName compiledDecl)
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


classificationInputs :: [CompiledItem] -> [(String, CExp)]
classificationInputs items =
  [ (compiledName decl, interfaceExp)
  | Compiled decl <- items
  , Just interfaceExp <- [compiledInterfaceIR decl]
  ]
