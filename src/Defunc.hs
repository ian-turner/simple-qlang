module Defunc (defunctionalize) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortOn)

import CPSExp
import Utils (Variable)


-- | Eliminate runtime code pointers from closure-converted CPS.
--
-- The current closure conversion pass represents function values as heap
-- records whose field 0 is a code label. Defunctionalization replaces those
-- labels with small integer tags and rewrites indirect calls of the form
--
--   SELECT(0, clo, tag, APP(tag, clo : args))
--
-- into a SWITCH on the tag with direct calls in each arm.
--
-- This pass is intentionally minimal: it handles the closure shape produced by
-- the current compiler and leaves direct VLabel calls unchanged.
defunctionalize :: CExp -> CExp
defunctionalize expr =
  let tagMap = closureTagMap expr
  in defuncExp tagMap expr


type TagMap = Map.Map String Int


-- | Assign tags to synthetic closure labels introduced by closure conversion.
closureTagMap :: CExp -> TagMap
closureTagMap expr =
  Map.fromList
    [ (lbl, i)
    | (lbl, i) <- zip (sortOn id (Set.toList (collectClosureLabels expr))) [0..]
    ]


defuncExp :: TagMap -> CExp -> CExp
defuncExp tagMap (CRecord fields v body) =
  CRecord (map (rewriteField tagMap) fields) v (defuncExp tagMap body)

defuncExp tagMap (CSelect 0 clo tag (CApp (VVar tag') (clo' : args)))
  | tag == tag'
  , clo == clo'
  , not (Map.null tagMap) =
      indirectDispatch tagMap clo tag args

defuncExp tagMap (CSelect i val v body) =
  CSelect i val v (defuncExp tagMap body)

defuncExp tagMap (COffset off val v body) =
  COffset off val v (defuncExp tagMap body)

defuncExp _ (CApp fn args) =
  CApp fn args

defuncExp tagMap (CFix defs body) =
  CFix [ (f, params, defuncExp tagMap defBody) | (f, params, defBody) <- defs ]
       (defuncExp tagMap body)

defuncExp tagMap (CSwitch val arms) =
  CSwitch val (map (defuncExp tagMap) arms)

defuncExp tagMap (CPrimOp op args results conts) =
  CPrimOp op args results (map (defuncExp tagMap) conts)

defuncExp tagMap (CFor idx lo hi body cont) =
  CFor idx
       (rewriteValue tagMap lo)
       (rewriteValue tagMap hi)
       (defuncExp tagMap body)
       (defuncExp tagMap cont)


rewriteField :: TagMap -> (Value, AccessPath) -> (Value, AccessPath)
rewriteField tagMap (val, path) =
  (rewriteValue tagMap val, path)


rewriteValue :: TagMap -> Value -> Value
rewriteValue tagMap (VLabel lbl) =
  case Map.lookup lbl tagMap of
    Just tag -> VInt tag
    Nothing  -> VLabel lbl
rewriteValue tagMap (VQubitArr base idx) = VQubitArr base (rewriteValue tagMap idx)
rewriteValue _ val = val


indirectDispatch :: TagMap -> Value -> Variable -> [Value] -> CExp
indirectDispatch tagMap clo tag args =
  CSwitch (VVar tag)
    [ CApp (VLabel lbl) (clo : args)
    | (lbl, _) <- sortOn snd (Map.toList tagMap)
    ]


collectClosureLabels :: CExp -> Set.Set String
collectClosureLabels (CRecord fields _ body) =
  Set.unions (closureLabelsInFields fields : [collectClosureLabels body])
collectClosureLabels (CSelect _ _ _ body) =
  collectClosureLabels body
collectClosureLabels (COffset _ _ _ body) =
  collectClosureLabels body
collectClosureLabels (CApp _ _) =
  Set.empty
collectClosureLabels (CFix defs body) =
  Set.unions (collectClosureLabels body : [collectClosureLabels b | (_, _, b) <- defs])
collectClosureLabels (CSwitch _ arms) =
  Set.unions (map collectClosureLabels arms)
collectClosureLabels (CPrimOp _ _ _ conts) =
  Set.unions (map collectClosureLabels conts)
collectClosureLabels (CFor _ _ _ body cont) =
  collectClosureLabels body `Set.union` collectClosureLabels cont


closureLabelsInFields :: [(Value, AccessPath)] -> Set.Set String
closureLabelsInFields =
  Set.fromList . mapMaybe labelInField
  where
    mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []
    labelInField (VLabel lbl, _) = Just lbl
    labelInField _               = Nothing
