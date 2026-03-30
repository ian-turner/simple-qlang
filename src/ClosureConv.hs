-- | Closure conversion pass  (Appel §10)
--
-- Eliminates all free variables from every function body so that functions
-- can be lifted to a single top-level CFix.  After this pass:
--
--   * No function has any free variables.
--   * All FIX definitions are collected into one top-level CFix.
--   * All references to those functions use VLabel (not VVar).
--
-- Strategy (conservative — all functions treated as escaping):
--
--   For each CFix group with n functions and m shared free variables, build
--   a single shared flat-closure record:
--
--     RECORD(code_0, code_1, ..., code_{n-1}, fv_0, ..., fv_{m-1})
--
--   Function i's closure pointer = OFFSET(shared_record, i), so that
--   SELECT(0, closure_i) gives code_i (standard calling convention).
--
--   In function i's body (closure param clo_i):
--     - free variable fv_j  → SELECT(n + j - i,   clo_i)
--     - sibling function f_k → OFFSET(k - i, clo_i) (negative offset OK)
--
--   Every VVar call is rewritten to fetch the code pointer and prepend the
--   closure:
--     CApp (VVar v) args  →  SELECT(0, v, cp, CApp cp (v : args))
--
module ClosureConv (closureConvert) where

import qualified Data.Set        as Set
import Control.Monad.State

import Utils    (Variable, freshNames)
import CPSExp


-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

type TopDef = (Variable, [Variable], CExp)
type CC     = State [TopDef]


-- ---------------------------------------------------------------------------
-- Free-variable analysis
-- ---------------------------------------------------------------------------

-- | Free variables of a CExp (only VVar counts; VLabel is a static label).
freeVars :: CExp -> Set.Set Variable
freeVars (CRecord fields v body) =
  Set.unions (map (fvVal . fst) fields)
  `Set.union` Set.delete v (freeVars body)
freeVars (CSelect _ val v body) =
  fvVal val `Set.union` Set.delete v (freeVars body)
freeVars (COffset _ val v body) =
  fvVal val `Set.union` Set.delete v (freeVars body)
freeVars (CApp fn args) =
  Set.unions (map fvVal (fn : args))
freeVars (CFix defs body) =
  let names  = Set.fromList (map (\(f,_,_) -> f) defs)
      defFvs = Set.unions
                 [ freeVars b `Set.difference` Set.fromList ps
                 | (_, ps, b) <- defs ]
  in (defFvs `Set.union` freeVars body) `Set.difference` names
freeVars (CSwitch val arms) =
  fvVal val `Set.union` Set.unions (map freeVars arms)
freeVars (CPrimOp _ args results conts) =
  Set.unions (map fvVal args)
  `Set.union` Set.unions
    [ freeVars c `Set.difference` Set.fromList results | c <- conts ]

fvVal :: Value -> Set.Set Variable
fvVal (VVar v) = Set.singleton v
fvVal _        = Set.empty


-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Closure-convert a CExp.  Returns a CExp whose only CFix (if any) is a
--   single top-level CFix containing all lifted function definitions.
closureConvert :: CExp -> CExp
closureConvert expr =
  let (body', topDefs) = runState (ccExp expr) []
  in case topDefs of
       []   -> body'
       defs -> CFix defs body'


-- ---------------------------------------------------------------------------
-- Main recursive conversion
-- ---------------------------------------------------------------------------

ccExp :: CExp -> CC CExp

ccExp (CRecord fields v body) = do
  body' <- ccExp body
  return $ CRecord fields v body'

ccExp (CSelect i val v body) = do
  body' <- ccExp body
  return $ CSelect i val v body'

ccExp (COffset off val v body) = do
  body' <- ccExp body
  return $ COffset off val v body'

ccExp (CSwitch val arms) = do
  arms' <- mapM ccExp arms
  return $ CSwitch val arms'

ccExp (CPrimOp op args results conts) = do
  conts' <- mapM ccExp conts
  return $ CPrimOp op args results conts'

-- Escaping call through a variable: fetch code pointer, pass closure as
-- first argument.
--   CApp (VVar v) args  →  SELECT(0, v, cp, CApp cp (v : args))
ccExp (CApp (VVar v) args) =
  freshNames ["_cp"] $ \[cp] ->
    return $ CSelect 0 (VVar v) cp
               (CApp (VVar cp) (VVar v : args))

-- VLabel call (e.g. "halt"): no closure needed, pass through unchanged.
ccExp (CApp fn args) =
  return $ CApp fn args

-- FIX group: the main case.
ccExp (CFix defs body) = do
  let names    = map (\(f,_,_) -> f) defs
      namesSet = Set.fromList names
      n        = length defs

  -- Free variables of each function body (excluding siblings and own params).
  let fvsPerFunc =
        [ Set.toList $ freeVars b
            `Set.difference` namesSet
            `Set.difference` Set.fromList params
        | (_, params, b) <- defs ]

  -- Shared free variables: stable ordering via Set.toList on the union.
  let sharedFvsSet = Set.unions (map Set.fromList fvsPerFunc)
      sharedFvs    = Set.toList sharedFvsSet
      m            = length sharedFvs

  -- Fresh code-label variables (one per function in this FIX group).
  freshNames (replicate n "lbl") $ \codeVars -> do

    -- Lift each function to top level.
    mapM_ (liftFunc n names sharedFvs m codeVars) (zip3 [0..] codeVars defs)

    -- Build the closure-creation code in the continuation.
    body' <- ccExp body
    freshNames ["rec"] $ \[recVar] -> do
      let recordFields =
              [ (VLabel (show cv), OFFp 0) | cv <- codeVars ]
           ++ [ (VVar fvj,        OFFp 0) | fvj <- sharedFvs ]
          -- Bind each function name to its OFFSET into the shared record.
          bindFunctions =
              foldr (\(i, fname) acc -> COffset i (VVar recVar) fname acc)
                    body'
                    (zip [0..] names)
      return $ CRecord recordFields recVar bindFunctions


-- | Convert a single function from a FIX group and add it to the top-level
--   accumulator.
liftFunc :: Int          -- n: total functions in this FIX group
         -> [Variable]   -- names: all sibling names (in order)
         -> [Variable]   -- sharedFvs: shared free variables (in order)
         -> Int          -- m: number of shared free variables
         -> [Variable]   -- codeVars: fresh code-label variables (in order)
         -> (Int, Variable, (Variable, [Variable], CExp))
         -> CC ()
liftFunc n names sharedFvs _m _codeVars (i, codeVar, (_fname, params, b)) =
  freshNames ["clo"] $ \[cloI] -> do
    b' <- ccExp b
    let newBody = buildPreamble i n cloI names sharedFvs b'
    modify ((codeVar, cloI : params, newBody) :)


-- | Wrap a function body with SELECT/OFFSET bindings that make all free
--   variables and sibling function closures available.
--
--   For function i in a group of n, with closure parameter cloI:
--     - Each sibling k  → OFFSET(k - i, cloI, f_k, ...)   (k == i gives self)
--     - Each fv_j       → SELECT(n + j - i, cloI, fv_j, ...)
buildPreamble
  :: Int        -- i: this function's index
  -> Int        -- n: number of sibling functions
  -> Variable   -- cloI: this function's closure parameter
  -> [Variable] -- names: sibling function names (in index order)
  -> [Variable] -- sharedFvs: shared free variable names (in index order)
  -> CExp       -- body: already-converted function body
  -> CExp
buildPreamble i n cloI names sharedFvs body =
  -- Innermost: bind free variables via SELECT
  let bindFvs =
        foldr (\(j, fvj) acc -> CSelect (n + j - i) (VVar cloI) fvj acc)
              body
              (zip [0..] sharedFvs)
      -- Outermost: bind sibling closures (and self) via OFFSET
      bindSiblings =
        foldr (\(k, sib) acc -> COffset (k - i) (VVar cloI) sib acc)
              bindFvs
              (zip [0..] names)
  in bindSiblings
