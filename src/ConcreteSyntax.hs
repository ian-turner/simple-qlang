module ConcreteSyntax where


data Exp
  = Unit
  | Num Integer
  | Var String
  | Tuple [Exp]
  | App Exp Exp
  | Let String Exp Exp
  | LetTuple [String] Exp Exp
  | Lam [String] Exp
  | IfExp Exp Exp Exp
  deriving (Show, Eq)

data Decl
  = VarDecl String Exp
  | FunDecl String [String] Exp
  deriving (Show, Eq)
