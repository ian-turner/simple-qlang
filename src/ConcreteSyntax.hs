module ConcreteSyntax where


data LetBinding
  = BSingle String Exp
  | BTuple [String] Exp
  deriving (Show, Eq)

data Exp
  = Unit                                -- Unit type and value
  | Var String                          -- Variable identifiers
  | Tuple [Exp]                         -- Tuple of objects (any size)
  | App Exp Exp                         -- Function application
  | Let [LetBinding] Exp                -- Let expression
  | Lam [String] Exp                    -- Lambda expressions
  | IfExp Exp Exp Exp                   -- If/then/else expressions
  deriving (Show, Eq)

data Decl
  = VarDef String Exp                   -- Variable declarations
  | FunDef String [String] Exp          -- Function declarations
  deriving (Show, Eq)
