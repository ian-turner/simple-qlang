module GateDef
  ( CallableKind(..)
  , ModuleCallableKinds(..)
  , analyzeModuleCallableKinds
  , lookupTopLevelCallableKind
  , renderCallableKind
  , renderModuleCallableKinds
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map

import CPSExp
import LambdaIR (PrimOp(..))
import Utils (Variable)


data CallableKind
  = CallableGate
  | CallableDef
  deriving (Eq, Ord, Show)


data ModuleCallableKinds = ModuleCallableKinds
  { moduleCallableKinds :: Map.Map String CallableKind
  }


analyzeModuleCallableKinds :: [(String, CExp)] -> ModuleCallableKinds
analyzeModuleCallableKinds decls =
  let initKinds =
        Map.fromList
          [ (name, CallableGate)
          | (name, _) <- decls
          ]
      step kinds =
        Map.fromList
          [ (name, classifyDecl kinds expr)
          | (name, expr) <- decls
          ]
      loop kinds =
        let kinds' = step kinds
        in if kinds' == kinds
             then kinds
             else loop kinds'
  in ModuleCallableKinds { moduleCallableKinds = loop initKinds }


lookupTopLevelCallableKind :: ModuleCallableKinds -> String -> Maybe CallableKind
lookupTopLevelCallableKind kinds name =
  Map.lookup name (moduleCallableKinds kinds)


renderCallableKind :: CallableKind -> String
renderCallableKind CallableGate = "gate"
renderCallableKind CallableDef  = "def"


renderModuleCallableKinds :: ModuleCallableKinds -> [String]
renderModuleCallableKinds kinds =
  let entries = List.sortOn fst (Map.toList (moduleCallableKinds kinds))
  in case entries of
       [] -> ["  (no callable top-level functions classified)"]
       _  ->
         [ "  " ++ name ++ " : " ++ renderCallableKind kind
         | (name, kind) <- entries
         ]


classifyDecl :: Map.Map String CallableKind -> CExp -> CallableKind
classifyDecl topKinds expr =
  case exportedBody expr of
    Just body ->
      let localBodies = collectLocalBodies body
          localKinds = iterateLocalKinds topKinds localBodies
      in mergeKinds (classifyExp topKinds localKinds body : Map.elems localKinds)
    Nothing ->
      CallableDef


exportedBody :: CExp -> Maybe CExp
exportedBody (CFix defs (CApp (VLabel "halt") [VVar exported])) =
  lookup exported
    [ (f, body)
    | (f, _, body) <- defs
    ]
exportedBody _ =
  Nothing


collectLocalBodies :: CExp -> Map.Map Variable CExp
collectLocalBodies (CRecord _ _ body) =
  collectLocalBodies body
collectLocalBodies (CSelect _ _ _ body) =
  collectLocalBodies body
collectLocalBodies (COffset _ _ _ body) =
  collectLocalBodies body
collectLocalBodies (CApp _ _) =
  Map.empty
collectLocalBodies (CFix defs body) =
  Map.unions
    ( Map.fromList [ (f, defBody) | (f, _, defBody) <- defs ]
    : collectLocalBodies body
    : [ collectLocalBodies defBody | (_, _, defBody) <- defs ]
    )
collectLocalBodies (CSwitch _ arms) =
  Map.unions (map collectLocalBodies arms)
collectLocalBodies (CPrimOp _ _ _ conts) =
  Map.unions (map collectLocalBodies conts)


iterateLocalKinds
  :: Map.Map String CallableKind
  -> Map.Map Variable CExp
  -> Map.Map Variable CallableKind
iterateLocalKinds topKinds localBodies =
  let initKinds = Map.map (const CallableGate) localBodies
      step kinds =
        Map.map (classifyExp topKinds kinds) localBodies
      loop kinds =
        let kinds' = step kinds
        in if kinds' == kinds
             then kinds
             else loop kinds'
  in loop initKinds


classifyExp
  :: Map.Map String CallableKind
  -> Map.Map Variable CallableKind
  -> CExp
  -> CallableKind
classifyExp topKinds localKinds (CRecord _ _ body) =
  classifyExp topKinds localKinds body
classifyExp topKinds localKinds (CSelect _ _ _ body) =
  classifyExp topKinds localKinds body
classifyExp topKinds localKinds (COffset _ _ _ body) =
  classifyExp topKinds localKinds body
classifyExp topKinds localKinds (CApp fn _) =
  calleeKind topKinds localKinds fn
classifyExp topKinds localKinds (CFix _ body) =
  classifyExp topKinds localKinds body
classifyExp _ _ (CSwitch _ _) =
  CallableDef
classifyExp topKinds localKinds (CPrimOp op _ _ conts)
  | gateCompatiblePrim op =
      mergeKinds (map (classifyExp topKinds localKinds) conts)
  | otherwise =
      CallableDef


calleeKind
  :: Map.Map String CallableKind
  -> Map.Map Variable CallableKind
  -> Value
  -> CallableKind
calleeKind _ _ (VLabel "halt") =
  CallableGate
calleeKind topKinds _ (VLabel name) =
  Map.findWithDefault CallableDef name topKinds
calleeKind _ localKinds (VVar v) =
  Map.findWithDefault CallableGate v localKinds
calleeKind _ _ _ =
  CallableGate


mergeKinds :: [CallableKind] -> CallableKind
mergeKinds kinds
  | CallableDef `elem` kinds = CallableDef
  | otherwise                = CallableGate


gateCompatiblePrim :: PrimOp -> Bool
gateCompatiblePrim PHGate  = True
gateCompatiblePrim PXGate  = True
gateCompatiblePrim PZGate  = True
gateCompatiblePrim PCNot   = True
gateCompatiblePrim PSGate  = True
gateCompatiblePrim PTGate  = True
gateCompatiblePrim PCSGate = True
gateCompatiblePrim PCTGate = True
gateCompatiblePrim PCpGate = True
gateCompatiblePrim _       = False
