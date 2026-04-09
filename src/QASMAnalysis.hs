module QASMAnalysis
  ( bodyAllocatesQubits
  , bodyIsSelfRecursive
  , isTailLoop
  ) where

import qualified Data.Map.Strict as Map

import CPSExp
import LambdaIR (PrimOp (..))
import Utils (Variable)


-- | True when the function is a "tail loop": every recursive self-call's
-- effective continuation is the outer continuation parameter, either passed
-- directly (VVar contParam) or via a locally-bound η-trivial wrapper.
--
-- Appel's CPS transform for application always introduces an intermediate
-- result continuation `r` bound by a local CFix with body
--   (r, [x], CApp (VVar k) [VVar x])
-- — i.e., a transparent pass-through to `k`.  Such wrappers are η-trivial
-- and should be treated as equivalent to passing `k` directly.
--
-- Functions that modify the continuation at the call site (e.g. init_n, which
-- builds a growing Cons chain through successive continuations) are NOT tail
-- loops and must instead be compiled by inline expansion.
isTailLoop :: String -> [Variable] -> CExp -> Bool
isTailLoop name params body =
  case params of
    [] -> False
    _ ->
      let contParam = last params
          calls = findSelfCallsInScope Map.empty name body
      in not (null calls) && all (effectiveContinuationIs contParam) calls
  where
    -- Collect recursive call arg-lists, threading the set of locally-bound
    -- CFix definitions seen on the way down (so we can resolve VVar wrappers).
    findSelfCallsInScope localDefs n (CRecord _ _ b) =
      findSelfCallsInScope localDefs n b
    findSelfCallsInScope localDefs n (CSelect _ _ _ b) =
      findSelfCallsInScope localDefs n b
    findSelfCallsInScope localDefs n (COffset _ _ _ b) =
      findSelfCallsInScope localDefs n b
    findSelfCallsInScope localDefs n (CApp (VLabel l) args)
      | l == n = [(localDefs, args)]
    findSelfCallsInScope _ _ (CApp _ _) = []
    findSelfCallsInScope localDefs n (CFix defs b) =
      let localDefs' = Map.union (Map.fromList [(f, (ps, fb)) | (f, ps, fb) <- defs]) localDefs
      in concatMap (\(_, _, db) -> findSelfCallsInScope localDefs' n db) defs
           ++ findSelfCallsInScope localDefs' n b
    findSelfCallsInScope localDefs n (CSwitch _ arms) =
      concatMap (findSelfCallsInScope localDefs n) arms
    findSelfCallsInScope localDefs n (CPrimOp _ _ _ conts) =
      concatMap (findSelfCallsInScope localDefs n) conts
    findSelfCallsInScope localDefs n (CFor _ _ _ forBody forCont) =
      findSelfCallsInScope localDefs n forBody
        ++ findSelfCallsInScope localDefs n forCont

    -- The effective continuation of a call is the outer contParam when the
    -- last arg is either:
    --   (a) VVar contParam directly, or
    --   (b) VVar w where w is locally bound to an η-trivial wrapper of contParam
    --       (possibly transitively through a chain of wrappers, as produced by
    --       Appel's LApp and LSwitch transforms which both introduce intermediate
    --       join/result continuations)
    effectiveContinuationIs contParam (localDefs, args) =
      case reverse args of
        (VVar v : _) -> reachesContParam localDefs v
        _            -> False
      where
        -- A variable "reaches the continuation" when:
        --   (a) it IS contParam directly,
        --   (b) it is not bound in localDefs — i.e. it is a free parameter
        --       such as the continuation k of an inner curried function (k2 in
        --       the two-level Appel curry encoding of f : A -> B -> C).  Such
        --       parameters are always eventually bound to the actual exit
        --       continuation by the time the function is fully applied.
        --   (c) it is bound to an η-trivial wrapper: (r,[x], CApp (VVar k) [VVar x])
        --   (d) it is bound to a curried-application continuation:
        --       (r,[x], CApp (VVar x) [..args..]) where the last arg reaches.
        reachesContParam _ v | v == contParam = True
        reachesContParam defs v =
          case Map.lookup v defs of
            Nothing ->
              -- Free variable: it's a parameter coming from outside this scope,
              -- i.e. a continuation endpoint such as k2 in a curried function body.
              -- NOTE: This is a known conservative over-approximation. If malformed
              -- CPS somehow placed a non-continuation free variable at the end of
              -- an η-chain, isTailLoop would incorrectly return True and compile a
              -- non-tail-loop function as a while loop. This is safe in practice
              -- because ToCPS only introduces free variables at continuation
              -- positions, but it is not verified structurally.
              True
            Just (ps, CApp (VVar cont) appArgs)
              | appArgs == map VVar ps ->
                  reachesContParam defs cont
            Just ([x], CApp (VVar appFun) appArgs)
              | appFun == x
              , not (null appArgs)
              , VVar lastArg <- last appArgs ->
                  reachesContParam defs lastArg
            _ -> False


-- | True if the CExp contains any direct self-call to the named top-level label.
bodyIsSelfRecursive :: String -> CExp -> Bool
bodyIsSelfRecursive name = go
  where
    go (CRecord _ _ body)         = go body
    go (CSelect _ _ _ body)       = go body
    go (COffset _ _ _ body)       = go body
    go (CApp (VLabel l) _)        = l == name
    go (CApp _ _)                 = False
    go (CFix defs body)           = any (\(_, _, b) -> go b) defs || go body
    go (CSwitch _ arms)           = any go arms
    go (CPrimOp _ _ _ conts)      = any go conts
    go (CFor _ _ _ body cont)     = go body || go cont


-- | True if the CExp contains any CPrimOp PInit (qubit allocation).
bodyAllocatesQubits :: CExp -> Bool
bodyAllocatesQubits = go
  where
    go (CRecord _ _ body)         = go body
    go (CSelect _ _ _ body)       = go body
    go (COffset _ _ _ body)       = go body
    go (CApp _ _)                 = False
    go (CFix defs body)           = any (\(_, _, b) -> go b) defs || go body
    go (CSwitch _ arms)           = any go arms
    go (CPrimOp PInit _ _ _)      = True
    go (CPrimOp _ _ _ conts)      = any go conts
    go (CFor _ _ _ body cont)     = go body || go cont
