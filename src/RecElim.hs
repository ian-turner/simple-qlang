-- | Recursion elimination pass  (Stage 5)
--
-- Checks the CPS expression (before closure conversion) for recursive
-- function definitions and reports a compile-time error if any are found.
--
-- Detection strategy: at each CFix, collect the set of bound function names,
-- then walk every function body with 'calleesInExp' (which returns all
-- Variable values that appear in CApp callee positions).  If the intersection
-- of bound names and callees is non-empty the group is recursive.
--
-- This pass is placed before closure conversion because recursive calls are
-- directly visible as CApp (VVar f) in the pre-closure CPS; after closure
-- conversion they are buried in COffset → CSelect → CApp chains.
--
-- For non-recursive programs the expression is returned unchanged.
-- Unrolling of bounded recursion is deferred to a later iteration of this
-- pass once the full pipeline is validated end-to-end.

module RecElim (elimRecursion) where

import qualified Data.Set as Set

import Utils  (Variable)
import CPSExp


-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Verify that the program contains no recursive function definitions.
-- Returns Right e (unchanged) on success, Left message on failure.
elimRecursion :: CExp -> Either String CExp
elimRecursion e = checkExp e >> return e


-- ---------------------------------------------------------------------------
-- Recursive checker
-- ---------------------------------------------------------------------------

checkExp :: CExp -> Either String ()
checkExp (CRecord _ _ body)         = checkExp body
checkExp (CSelect _ _ _ body)       = checkExp body
checkExp (COffset _ _ _ body)       = checkExp body
checkExp (CApp _ _)                 = return ()
checkExp (CSwitch _ arms)           = mapM_ checkExp arms
checkExp (CPrimOp _ _ _ conts)      = mapM_ checkExp conts
checkExp (CFix defs body) = do
  let names = Set.fromList [f | (f, _, _) <- defs]
  mapM_ (checkDef names) defs
  mapM_ (\(_, _, b) -> checkExp b) defs
  checkExp body

-- | Check one function in a CFix group for calls back into the group.
checkDef :: Set.Set Variable -> (Variable, [Variable], CExp) -> Either String ()
checkDef groupNames (fname, _, defBody) =
  let called    = calleesInExp defBody
      recursive = Set.intersection groupNames called
  in if Set.null recursive
       then return ()
       else Left $
              "RecElim: recursive function '" ++ show fname
              ++ "' is not yet supported — unbounded recursion cannot be "
              ++ "emitted as OpenQASM.  Supply a static bound or restructure "
              ++ "the algorithm."


-- ---------------------------------------------------------------------------
-- Callee collection
-- ---------------------------------------------------------------------------

-- | Collect all Variable values that appear in the callee position of CApp
-- anywhere within the expression (including inside nested CFix bodies).
calleesInExp :: CExp -> Set.Set Variable
calleesInExp (CRecord _ _ body)       = calleesInExp body
calleesInExp (CSelect _ _ _ body)     = calleesInExp body
calleesInExp (COffset _ _ _ body)     = calleesInExp body
calleesInExp (CApp (VVar v) _)        = Set.singleton v
calleesInExp (CApp _ _)               = Set.empty
calleesInExp (CSwitch _ arms)         = Set.unions (map calleesInExp arms)
calleesInExp (CPrimOp _ _ _ conts)    = Set.unions (map calleesInExp conts)
calleesInExp (CFix defs body)         =
  Set.unions (calleesInExp body : [calleesInExp b | (_, _, b) <- defs])
