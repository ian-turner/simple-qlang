module PrettyPrint
  ( PP(..)
  , ppShow
  ) where

import Prelude hiding ((<>))
import Text.PrettyPrint
import Nominal

import Utils (Variable(..))
import LambdaIR
import CPSExp
import QubitHoist (HoistedProgram(..))
import qualified ConcreteSyntax as C
import qualified Syntax as S


-- ---------------------------------------------------------------------------
-- Class and helpers

class PP a where
  pp :: a -> Doc

ppShow :: PP a => a -> String
ppShow = render . pp

ind :: Int
ind = 2

ppCommaSep :: [Doc] -> Doc
ppCommaSep = vcat . punctuate comma

ppArrow :: Doc
ppArrow = text "->"

ppFatArrow :: Doc
ppFatArrow = text "=>"

ppIn :: Doc -> Doc
ppIn body = text "in" $$ nest ind body

ppTuple :: [Doc] -> Doc
ppTuple [d] = d
ppTuple ds  = parens (hcat (punctuate (text ", ") ds))

ppArgs :: [Doc] -> Doc
ppArgs ds = parens (hcat (punctuate (text ", ") ds))


-- ---------------------------------------------------------------------------
-- Leaves

instance PP Variable where
  pp (Variable a (NoBind hint)) = text (hint ++ "@" ++ show a)

instance PP PrimOp where
  pp PInit   = text "init"
  pp PMeas   = text "meas"
  pp PHGate  = text "H"
  pp PXGate  = text "X"
  pp PZGate  = text "Z"
  pp PCNot   = text "CNOT"
  pp PSGate  = text "S"
  pp PTGate  = text "T"
  pp PCSGate = text "CS"
  pp PCTGate = text "CT"
  pp PCpGate = text "CP"
  pp PAdd    = text "+"
  pp PSub    = text "-"
  pp PMul    = text "*"
  pp PDiv    = text "/"
  pp PEq     = text "=="
  pp PLt     = text "<"
  pp PGt     = text ">"
  pp PLe     = text "<="
  pp PGe     = text ">="
  pp PAnd    = text "&&"
  pp POr     = text "||"
  pp PNot    = text "!"

instance PP Lit where
  pp (LInt n)    = int n
  pp (LFloat s)  = text s
  pp (LBool b)   = text (if b then "True" else "False")
  pp (LString s) = text (show s)
  pp LUnit       = text "()"

instance PP ConAlt where
  pp (CACon s) = text s
  pp CAUnit    = text "()"


-- ---------------------------------------------------------------------------
-- LambdaIR

instance PP LExp where
  pp = ppLExp

ppLExp :: LExp -> Doc
ppLExp (LVar v)     = pp v
ppLExp (LLit l)     = pp l
ppLExp (LTopVar s)  = text s
ppLExp e@(LApp _ _) =
  let (f, args) = spine e
  in vcat (ppLExp f : map (nest ind . ppLAtom) args)
  where
    spine (LApp g x) = let (h, as) = spine g in (h, as ++ [x])
    spine e0 = (e0, [])
ppLExp e@(LLam _ _) =
  let (vs, body) = lams e
  in (text "λ" <> hsep (map pp vs) <> text ".") $$ nest ind (ppLExp body)
  where
    lams (LLam v b) = let (vs, body) = lams b in (v:vs, body)
    lams e0 = ([], e0)
ppLExp (LFix binds body) =
  text "fix" $$
  nest ind (vcat (map ppLFixBind binds)) $$
  ppIn (ppLExp body)
ppLExp (LTuple es) =
  parens (ppCommaSep (map ppLExp es))
ppLExp (LSelect i e) =
  ppLAtom e <> text ("." ++ show i)
ppLExp (LCon c e) =
  text c <> parens (ppLExp e)
ppLExp (LDecon c e) =
  text "decon" <+> text c <+> ppLAtom e
ppLExp (LSwitch e arms dflt) =
  (text "switch" <+> ppLAtom e <+> lbrace) $$
  nest ind (vcat (map ppArm arms ++ dfltDoc)) $$
  rbrace
  where
    ppArm (alt, rhs) = (pp alt <+> ppFatArrow) $$ nest ind (ppLExp rhs)
    dfltDoc = case dflt of
      Nothing -> []
      Just d  -> [(text "_" <+> ppFatArrow) $$ nest ind (ppLExp d)]
ppLExp (LPrim op args) =
  pp op <> ppArgs (map ppLExp args)

ppLFixBind :: (Variable, Variable, LExp) -> Doc
ppLFixBind (f, x, body) =
  (pp f <> parens (pp x) <+> text "=") $$
  nest ind (ppLExp body)

-- Atomic: no parens needed for vars/lits; everything else gets wrapped
ppLAtom :: LExp -> Doc
ppLAtom e@(LVar _)    = ppLExp e
ppLAtom e@(LLit _)    = ppLExp e
ppLAtom e@(LTopVar _) = ppLExp e
ppLAtom e             = parens (ppLExp e)


-- ---------------------------------------------------------------------------
-- CPSExp

instance PP AccessPath where
  pp (OFFp n)      = text ("." ++ show n)
  pp (SELp i path) = text ("." ++ show i) <> pp path

instance PP Value where
  pp (VVar v)          = pp v
  pp (VLabel s)        = text ("@" ++ s)
  pp (VInt n)          = int n
  pp (VFloat s)        = text s
  pp (VQubit n)        = text ("q[" ++ show n ++ "]")
  pp (VQubitArr b idx) = text ("q[" ++ show b ++ "+") <> pp idx <> text "]"
  pp VUnit             = text "()"

instance PP CExp where
  pp = ppCExp

ppCExp :: CExp -> Doc
ppCExp (CRecord fields x body) =
  (text "let" <+> pp x <+> text "= record" <+>
   brackets (hcat (punctuate (text ", ") (map ppField fields))) <+>
   text "in") $$
  nest ind (ppCExp body)
  where
    ppField (v, path) = pp v <> pp path
ppCExp (CSelect i v x body) =
  (text "let" <+> pp x <+> text "=" <+> pp v <> text ("." ++ show i) <+> text "in") $$
  nest ind (ppCExp body)
ppCExp (CApp f args) =
  pp f <> ppArgs (map pp args)
ppCExp (CFix binds body) =
  text "fix" $$
  nest ind (vcat (map ppCFixBind binds)) $$
  ppIn (ppCExp body)
ppCExp (CSwitch v arms) =
  (text "switch" <+> pp v <+> lbrace) $$
  nest ind (vcat (zipWith ppArm [0 :: Int ..] arms)) $$
  rbrace
  where
    ppArm i arm = (int i <+> ppFatArrow) $$ nest ind (ppCExp arm)
ppCExp (CPrimOp op args results conts) =
  (text "let" <+> ppTuple (map pp results) <+>
   text "= " <> pp op <> ppArgs (map pp args) <+> text "in") $$
  nest ind (vcat (map ppCExp conts))
ppCExp (COffset n v x body) =
  (text "let" <+> pp x <+> text "= offset" <+> int n <+> pp v <+> text "in") $$
  nest ind (ppCExp body)
ppCExp (CFor i lo hi loopBody exitBody) =
  (text "for" <+> pp i <+> text "in [" <>
   pp lo <> text ".." <> pp hi <> text "] {") $$
  nest ind (ppCExp loopBody) $$
  rbrace $$
  ppCExp exitBody

ppCFixBind :: (Variable, [Variable], CExp) -> Doc
ppCFixBind (f, params, body) =
  (pp f <> ppArgs (map pp params) <+> text "=") $$
  nest ind (ppCExp body)

instance PP HoistedProgram where
  pp (HoistedProgram qubits body) =
    (text "qubits:" <+> int qubits) $$ pp body


-- ---------------------------------------------------------------------------
-- ConcreteSyntax

instance PP C.FlatArg where
  pp (C.FArg s) = text s
  pp C.FWild    = text "_"

instance PP C.Pat where
  pp (C.PVar s)      = text s
  pp C.PWild         = text "_"
  pp (C.PCon c args) = text c <+> hsep (map pp args)
  pp (C.PTuple args) = parens (hcat (punctuate (text ", ") (map pp args)))
  pp C.PUnit         = text "()"

instance PP C.LetBinding where
  pp (C.BSingle s e) =
    (text s <+> text "=") $$ nest ind (pp e)
  pp (C.BTuple ss e) =
    (parens (hcat (punctuate (text ", ") (map text ss))) <+> text "=") $$
    nest ind (pp e)

instance PP C.Exp where
  pp C.Unit          = text "()"
  pp (C.NumInt n)    = int n
  pp (C.NumFloat s)  = text s
  pp (C.BoolLit b)   = text (if b then "True" else "False")
  pp (C.StringLit s) = text (show s)
  pp (C.Var s)       = text s
  pp (C.Tuple es)    = parens (ppCommaSep (map pp es))
  pp e@(C.App _ _)   =
    let (f, args) = spine e
    in vcat (ppCAtom f : map (nest ind . ppCAtom) args)
    where
      spine (C.App g x) = let (h, as) = spine g in (h, as ++ [x])
      spine e0 = (e0, [])
  pp (C.BinOp op l r) =
    ppCAtom l <+> text op $$ nest ind (ppCAtom r)
  pp (C.Let binds body) =
    text "let" $$
    nest ind (vcat (map pp binds)) $$
    ppIn (pp body)
  pp (C.Lam args body) =
    (text "λ" <> hsep (map text args) <> text ".") $$
    nest ind (pp body)
  pp (C.IfExp cond t f) =
    (text "if" $$ nest ind (pp cond)) $$
    (text "then" $$ nest ind (pp t)) $$
    (text "else" $$ nest ind (pp f))
  pp (C.CaseExp e alts) =
    (text "case" <+> ppCAtom e <+> text "of") $$
    nest ind (vcat (map ppAlt alts))
    where ppAlt (pat, rhs) = (pp pat <+> ppFatArrow) $$ nest ind (pp rhs)
  pp C.Dynlift = text "dynlift"

ppCAtom :: C.Exp -> Doc
ppCAtom e@(C.Var _)      = pp e
ppCAtom e@(C.NumInt _)   = pp e
ppCAtom e@(C.NumFloat _) = pp e
ppCAtom e@(C.BoolLit _)  = pp e
ppCAtom e@(C.StringLit _)= pp e
ppCAtom C.Unit           = text "()"
ppCAtom e                = parens (pp e)

instance PP C.TypeExp where
  pp (C.TyVar s)     = text s
  pp (C.TyCon s)     = text s
  pp (C.TyApp t1 t2) = pp t1 $$ nest ind (ppCTyAtom t2)
  pp (C.TyFun t1 t2) = ppCTyArg t1 <+> ppArrow $$ nest ind (pp t2)
  pp (C.TyTuple ts)  = parens (hcat (punctuate (text ", ") (map pp ts)))

ppCTyAtom :: C.TypeExp -> Doc
ppCTyAtom e@(C.TyVar _)   = pp e
ppCTyAtom e@(C.TyCon _)   = pp e
ppCTyAtom e@(C.TyTuple _) = pp e
ppCTyAtom e               = parens (pp e)

ppCTyArg :: C.TypeExp -> Doc
ppCTyArg e@(C.TyFun _ _) = parens (pp e)
ppCTyArg e               = pp e

instance PP C.ConDecl where
  pp (C.ConDecl c ts) = text c <+> hsep (map ppCTyAtom ts)

instance PP C.Decl where
  pp (C.VarDef s e) =
    (text s <+> text "=") $$ nest ind (pp e)
  pp (C.FunDef s args e) =
    (text s <+> hsep (map text args) <+> text "=") $$
    nest ind (pp e)
  pp (C.TypeSig s t) =
    text s <+> text ":" <+> pp t
  pp (C.DataDecl name vars cons) =
    (text "data" <+> text name <+> hsep (map text vars) <+> text "=") $$
    nest ind (vcat (punctuate (text " |") (map pp cons)))


-- ---------------------------------------------------------------------------
-- Syntax (resolved AST, uses Nominal bindings)

instance PP S.PatField where
  pp (S.PFVar v) = pp v
  pp S.PFWild    = text "_"

instance PP S.Pat where
  pp (S.PVar v)      = pp v
  pp S.PWild         = text "_"
  pp (S.PCon c args) = text c <+> hsep (map pp args)
  pp (S.PTuple args) = parens (hcat (punctuate (text ", ") (map pp args)))
  pp S.PUnit         = text "()"

instance PP S.Exp where
  pp S.Unit          = text "()"
  pp (S.NumInt n)    = int n
  pp (S.NumFloat s)  = text s
  pp (S.BoolLit b)   = text (if b then "True" else "False")
  pp (S.StringLit s) = text (show s)
  pp (S.Var v)       = pp v
  pp (S.Const s)     = text s
  pp (S.Base s)      = text ("@" ++ s)
  pp (S.Con s)       = text s
  pp (S.Tuple es)    = parens (ppCommaSep (map pp es))
  pp e@(S.App _ _)   =
    let (f, args) = spine e
    in vcat (ppSAtom f : map (nest ind . ppSAtom) args)
    where
      spine (S.App g x) = let (h, as) = spine g in (h, as ++ [x])
      spine e0 = (e0, [])
  pp (S.Lam bnd) =
    case bnd of
      (vs :. body) ->
        (text "λ" <> hsep (map pp vs) <> text ".") $$
        nest ind (pp body)
  pp (S.Let e bnd) =
    case bnd of
      (v :. body) ->
        (text "let" <+> pp v <+> text "=") $$
        nest ind (pp e) $$
        ppIn (pp body)
  pp (S.LetTuple e bnd) =
    case bnd of
      (vs :. body) ->
        (text "let" <+>
         parens (hcat (punctuate (text ", ") (map pp vs))) <+>
         text "=") $$
        nest ind (pp e) $$
        ppIn (pp body)
  pp (S.IfExp cond t f) =
    (text "if" $$ nest ind (pp cond)) $$
    (text "then" $$ nest ind (pp t)) $$
    (text "else" $$ nest ind (pp f))
  pp (S.Case e alts) =
    (text "case" <+> ppSAtom e <+> text "of") $$
    nest ind (vcat (map pp alts))
  pp S.Dynlift = text "dynlift"

instance PP S.Alt where
  pp (S.Alt bnd) =
    case bnd of
      (_vs :. (pat, rhs)) ->
        (pp pat <+> ppFatArrow) $$ nest ind (pp rhs)

ppSAtom :: S.Exp -> Doc
ppSAtom e@(S.Var _)      = pp e
ppSAtom e@(S.Const _)    = pp e
ppSAtom e@(S.Base _)     = pp e
ppSAtom e@(S.Con _)      = pp e
ppSAtom e@(S.NumInt _)   = pp e
ppSAtom e@(S.NumFloat _) = pp e
ppSAtom e@(S.BoolLit _)  = pp e
ppSAtom e@(S.StringLit _)= pp e
ppSAtom S.Unit           = text "()"
ppSAtom e                = parens (pp e)

instance PP S.TypeExp where
  pp (S.TyVar s)     = text s
  pp (S.TyCon s)     = text s
  pp (S.TyApp t1 t2) = pp t1 $$ nest ind (ppSTyAtom t2)
  pp (S.TyFun t1 t2) = ppSTyArg t1 <+> ppArrow $$ nest ind (pp t2)
  pp (S.TyTuple ts)  = parens (hcat (punctuate (text ", ") (map pp ts)))

ppSTyAtom :: S.TypeExp -> Doc
ppSTyAtom e@(S.TyVar _)   = pp e
ppSTyAtom e@(S.TyCon _)   = pp e
ppSTyAtom e@(S.TyTuple _) = pp e
ppSTyAtom e               = parens (pp e)

ppSTyArg :: S.TypeExp -> Doc
ppSTyArg e@(S.TyFun _ _) = parens (pp e)
ppSTyArg e               = pp e

instance PP S.ConDecl where
  pp (S.ConDecl c ts) = text c <+> hsep (map ppSTyAtom ts)

instance PP S.Decl where
  pp (S.Def s e) =
    (text s <+> text "=") $$ nest ind (pp e)
  pp (S.TypeSig s t) =
    text s <+> text ":" <+> pp t
  pp (S.DataDecl name vars cons) =
    (text "data" <+> text name <+> hsep (map text vars) <+> text "=") $$
    nest ind (vcat (punctuate (text " |") (map pp cons)))
