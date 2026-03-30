module ToCPS (toCPS, toCPSDecl) where

import qualified Data.Map.Strict as Map

import Utils (freshNames)
import LambdaIR
import CPSExp


-- | Metalanguage continuation: a Haskell function, not a CPS variable.
-- Exists only at compile time; threading "what to do with the result".
type Kont = Value -> CExp


-- ---------------------------------------------------------------------------
-- Main conversion function — Appel §5, F(E, c)
-- ---------------------------------------------------------------------------

toCPS :: LExp -> Kont -> CExp

-- §5.1  Variables and literals
toCPS (LVar v)            c = c (VVar v)
toCPS (LTopVar s)         c = c (VLabel s)
toCPS (LLit LUnit)        c = c VUnit
toCPS (LLit (LInt n))     c = c (VInt n)
toCPS (LLit (LBool b))    c = c (VInt (if b then 1 else 0))
toCPS (LLit (LFloat _))   c = c (VInt 0)   -- floats not yet in CPS IR
toCPS (LLit (LString _))  c = c (VInt 0)   -- strings not yet in CPS IR

-- §5.4  Lambda abstraction — gains extra continuation parameter k
--   F(λv.body, c) = FIX [(f, [v,k], F(body, λz.APP(k,[z])))] (c (VAR f))
toCPS (LLam v body)       c =
  freshNames ["f", "k"] $ \[f, k] ->
    CFix [(f, [v, k], toCPS body (\z -> CApp (VVar k) [z]))]
         (c (VVar f))

-- §5.4  Application
--   F(func arg, c) = FIX [(r,[x], c(x))] (F(func, λf. F(arg, λe. APP(f,[e,r]))))
toCPS (LApp func arg)     c =
  freshNames ["r", "x"] $ \[r, x] ->
    CFix [(r, [x], c (VVar x))]
         (toCPS func $ \f ->
          toCPS arg  $ \e ->
          CApp f [e, VVar r])

-- §5.5  Mutually recursive definitions
--   F(FIX defs body, c) = CFix (map cpsDef defs) (F(body, c))
toCPS (LFix defs body)    c =
  let cpsDef (fname, param, b) =
        freshNames ["w"] $ \[w] ->
          (fname, [param, w], toCPS b (\z -> CApp (VVar w) [z]))
  in CFix (map cpsDef defs) (toCPS body c)

-- §5.2  Tuple construction — allocates a heap record
toCPS (LTuple es)         c =
  toCPSList es $ \vs ->
    freshNames ["t"] $ \[t] ->
      CRecord (map (\v -> (v, OFFp 0)) vs) t (c (VVar t))

-- §5.2  Field selection
toCPS (LSelect i e)       c =
  toCPS e $ \v ->
    freshNames ["s"] $ \[s] ->
      CSelect i v s (c (VVar s))

-- §5.6  Constructor application
toCPS (LCon name e)       c =
  case conRepOf name of
    ConConstant tag ->
      -- Nullary constructor: ignore payload, represent as integer tag
      c (VInt tag)
    ConTagged tag ->
      -- Tagged constructor: allocate record (payload, tag)
      toCPS e $ \v ->
        freshNames ["r"] $ \[r] ->
          CRecord [(v, OFFp 0), (VInt tag, OFFp 0)] r (c (VVar r))
    ConTransparent ->
      -- Single-constructor type: pass the payload through unchanged
      toCPS e c

-- §5.6  Constructor deconstruction — select field 0 (the payload)
toCPS (LDecon _name e)    c =
  toCPS e $ \v ->
    freshNames ["d"] $ \[d] ->
      CSelect 0 v d (c (VVar d))

-- §5.7  Switch on constructor tag
toCPS (LSwitch scrut arms mbDef) c =
  toCPS scrut $ \v ->
    CSwitch v (buildSwitchArms arms mbDef c)

-- Primitive operations — dispatched by category
toCPS (LPrim op args)     c = toCPSPrim op args c


-- ---------------------------------------------------------------------------
-- Primitive operation conversion
-- ---------------------------------------------------------------------------

toCPSPrim :: PrimOp -> [LExp] -> Kont -> CExp

-- PMeas: category 3 — branches into two continuations (|0⟩ and |1⟩).
-- FIX-wraps c so the continuation body is generated exactly once.
--   F(meas e, c) = F(e, λv. FIX [(k,[x], c(x))]
--                            PRIMOP(PMeas,[v],[],[APP(k,[0]), APP(k,[1])]))
toCPSPrim PMeas args c =
  toCPSList args $ \vs ->
    freshNames ["k", "x"] $ \[k, x] ->
      CFix [(k, [x], c (VVar x))]
           (CPrimOp PMeas vs []
             [ CApp (VVar k) [VInt 0]   -- |0⟩ outcome
             , CApp (VVar k) [VInt 1]   -- |1⟩ outcome
             ])

-- PCNot: two output qubits — bind both, then pack into a record so that
-- the continuation c receives a single Value (the record address).
-- The RECORD/SELECT pair is eliminated by beta-contraction later.
toCPSPrim PCNot args c =
  toCPSList args $ \vs ->
    freshNames ["q1", "q2", "r"] $ \[q1', q2', r] ->
      CPrimOp PCNot vs [q1', q2']
        [ CRecord [(VVar q1', OFFp 0), (VVar q2', OFFp 0)] r (c (VVar r)) ]

-- All other primops: category 1 — one result variable, one continuation.
toCPSPrim op args c =
  toCPSList args $ \vs ->
    freshNames ["w"] $ \[w] ->
      CPrimOp op vs [w] [c (VVar w)]


-- ---------------------------------------------------------------------------
-- F_l: evaluate a list of expressions left-to-right (Appel §5.2)
-- ---------------------------------------------------------------------------

toCPSList :: [LExp] -> ([Value] -> CExp) -> CExp
toCPSList []     k = k []
toCPSList (e:es) k = toCPS e $ \v -> toCPSList es $ \vs -> k (v:vs)


-- ---------------------------------------------------------------------------
-- Top-level declaration conversion
-- ---------------------------------------------------------------------------

-- | Convert a top-level declaration body.
-- The outermost continuation is a call to the special 'halt' label,
-- representing program exit / result delivery to the caller.
toCPSDecl :: String -> LExp -> CExp
toCPSDecl _name body =
  toCPS body (\v -> CApp (VLabel "halt") [v])


-- ---------------------------------------------------------------------------
-- Constructor representation
-- ---------------------------------------------------------------------------

data ConRep
  = ConConstant Int   -- nullary: represented as an integer tag
  | ConTagged   Int   -- value-carrying: record (payload, tag)
  | ConTransparent    -- single-constructor: identity

conRepOf :: String -> ConRep
conRepOf "False" = ConConstant 0
conRepOf "True"  = ConConstant 1
conRepOf "Nil"   = ConConstant 0
conRepOf "Cons"  = ConTagged   1
conRepOf _       = ConTagged   0   -- user-defined; assume tagged, tag=0


-- | Integer tag for a ConAlt scrutinee arm.
conAltTag :: ConAlt -> Int
conAltTag (CACon "False") = 0
conAltTag (CACon "True")  = 1
conAltTag (CACon "Nil")   = 0
conAltTag (CACon "Cons")  = 1
conAltTag (CACon _)       = 0
conAltTag CAUnit          = 0


-- | Build the CSwitch arm list, filling gaps 0..maxTag from the default arm.
buildSwitchArms :: [(ConAlt, LExp)] -> Maybe LExp -> Kont -> [CExp]
buildSwitchArms arms mbDef c =
  let taggedArms = [ (conAltTag alt, body) | (alt, body) <- arms ]
      maxTag     = maximum (0 : map fst taggedArms)
      armMap     = foldr (\(t, b) m -> Map.insert t b m) Map.empty taggedArms
      lookupArm tag = case Map.lookup tag armMap of
        Just body -> toCPS body c
        Nothing   -> case mbDef of
          Just def -> toCPS def c
          Nothing  -> error ("ToCPS: incomplete switch, no arm for tag " ++ show tag)
  in map lookupArm [0..maxTag]
