module BoundedRecursion
  ( TopLevelFunction(..)
  , extractTopLevelFunction
  , CountedSelfRecursion(..)
  , extractCountedSelfRecursion
  ) where

import CPSExp
import LambdaIR (PrimOp(..))
import Utils (Variable)


data TopLevelFunction = TopLevelFunction
  { tlfParams :: [Variable]
  , tlfBody   :: CExp
  }


data CountedSelfRecursion = CountedSelfRecursion
  { csrParams :: [Variable]
  , csrBody   :: CExp
  }


extractCountedSelfRecursion :: String -> CExp -> Maybe CountedSelfRecursion
extractCountedSelfRecursion selfName expr = do
  TopLevelFunction params body <- extractTopLevelFunction expr
  case params of
    [counter, _k]
      | matchesCountedSelfRecursion selfName counter body ->
          Just CountedSelfRecursion
            { csrParams = params
            , csrBody = body
            }
    _ ->
      Nothing


extractTopLevelFunction :: CExp -> Maybe TopLevelFunction
extractTopLevelFunction (CFix [(funName, params, body)] (CApp (VLabel "halt") [VVar result]))
  | funName == result =
      Just TopLevelFunction
        { tlfParams = params
        , tlfBody = body
        }
extractTopLevelFunction _ =
  Nothing


matchesCountedSelfRecursion :: String -> Variable -> CExp -> Bool
matchesCountedSelfRecursion selfName counter expr =
  case expr of
    CPrimOp PEq [lhs, rhs] [cond] [CSwitch (VVar scrutinee) [recursiveArm, baseArm]]
      | cond == scrutinee
      , matchesCounterTest counter lhs rhs
      , not (containsSelfCall selfName baseArm)
      , matchesRecursiveArm selfName counter recursiveArm ->
          True
    CPrimOp PEq [lhs, rhs] [_] [cont]
      | matchesCounterTest counter lhs rhs ->
          matchesCountedSelfRecursion selfName counter cont
    CFix defs body ->
      all (\(_, _, defBody) -> matchesCountedSelfRecursion selfName counter defBody) defs
        && matchesCountedSelfRecursion selfName counter body
    CRecord _ _ body ->
      matchesCountedSelfRecursion selfName counter body
    CSelect _ _ _ body ->
      matchesCountedSelfRecursion selfName counter body
    COffset _ _ _ body ->
      matchesCountedSelfRecursion selfName counter body
    _ ->
      False


matchesCounterTest :: Variable -> Value -> Value -> Bool
matchesCounterTest counter (VVar v) (VInt 0) = v == counter
matchesCounterTest counter (VInt 0) (VVar v) = v == counter
matchesCounterTest _ _ _ = False


matchesRecursiveArm :: String -> Variable -> CExp -> Bool
matchesRecursiveArm selfName counter arm =
  case selfCallCounters selfName arm of
    [VVar nextCounter] ->
      bindsCountdownStep counter nextCounter arm
    _ ->
      False


selfCallCounters :: String -> CExp -> [Value]
selfCallCounters selfName expr =
  case expr of
    CRecord _ _ body ->
      selfCallCounters selfName body
    CSelect _ _ _ body ->
      selfCallCounters selfName body
    COffset _ _ _ body ->
      selfCallCounters selfName body
    CApp (VLabel name) (counterArg : _)
      | name == selfName ->
          [counterArg]
    CApp _ _ ->
      []
    CFix defs body ->
      concatMap (\(_, _, defBody) -> selfCallCounters selfName defBody) defs
        ++ selfCallCounters selfName body
    CSwitch _ arms ->
      concatMap (selfCallCounters selfName) arms
    CPrimOp _ _ _ conts ->
      concatMap (selfCallCounters selfName) conts


bindsCountdownStep :: Variable -> Variable -> CExp -> Bool
bindsCountdownStep counter nextCounter expr =
  case expr of
    CRecord _ _ body ->
      bindsCountdownStep counter nextCounter body
    CSelect _ _ _ body ->
      bindsCountdownStep counter nextCounter body
    COffset _ _ _ body ->
      bindsCountdownStep counter nextCounter body
    CApp _ _ ->
      False
    CFix defs body ->
      any (\(_, _, defBody) -> bindsCountdownStep counter nextCounter defBody) defs
        || bindsCountdownStep counter nextCounter body
    CSwitch _ arms ->
      any (bindsCountdownStep counter nextCounter) arms
    CPrimOp PSub [VVar source, VInt 1] [result] conts
      | source == counter && result == nextCounter ->
          True
      | otherwise ->
          any (bindsCountdownStep counter nextCounter) conts
    CPrimOp _ _ _ conts ->
      any (bindsCountdownStep counter nextCounter) conts


containsSelfCall :: String -> CExp -> Bool
containsSelfCall selfName expr =
  case expr of
    CRecord _ _ body ->
      containsSelfCall selfName body
    CSelect _ _ _ body ->
      containsSelfCall selfName body
    COffset _ _ _ body ->
      containsSelfCall selfName body
    CApp (VLabel name) _
      | name == selfName ->
          True
    CApp _ _ ->
      False
    CFix defs body ->
      any (\(_, _, defBody) -> containsSelfCall selfName defBody) defs
        || containsSelfCall selfName body
    CSwitch _ arms ->
      any (containsSelfCall selfName) arms
    CPrimOp _ _ _ conts ->
      any (containsSelfCall selfName) conts
