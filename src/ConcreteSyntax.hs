module ConcreteSyntax where


data LetBinding
  = BSingle String Exp
  | BTuple [String] Exp
  deriving (Show, Eq)

-- | Flat pattern argument: a bound variable or a wildcard
data FlatArg
  = FArg String                         -- bound variable
  | FWild                               -- wildcard _
  deriving (Show, Eq)

-- | Flat patterns (one level deep)
data Pat
  = PVar String                         -- variable pattern
  | PWild                               -- wildcard _
  | PCon String [FlatArg]               -- constructor pattern (flat args)
  | PTuple [FlatArg]                    -- tuple pattern (flat args)
  | PUnit                               -- unit pattern ()
  deriving (Show, Eq)

data Exp
  = Unit                                -- Unit type and value
  | NumInt Int                          -- Numbers
  | NumFloat Float
  | Var String                          -- Variable identifiers
  | Tuple [Exp]                         -- Tuple of objects (any size)
  | App Exp Exp                         -- Function application
  | Let [LetBinding] Exp                -- Let expression
  | Lam [String] Exp                    -- Lambda expressions
  | IfExp Exp Exp Exp                   -- If/then/else expressions
  | CaseExp Exp [(Pat, Exp)]            -- case expression
  | Dynlift                             -- Dynamic lifting function
  deriving (Show, Eq)

-- | Type expressions
data TypeExp
  = TyVar String                        -- Type variable (lowercase, e.g. a, b)
  | TyCon String                        -- Type constant (uppercase, e.g. Int, Qubit)
  | TyApp TypeExp TypeExp               -- Type application (e.g. Maybe a)
  | TyFun TypeExp TypeExp               -- Function type (e.g. a -> b)
  | TyTuple [TypeExp]                   -- Tuple type (e.g. (a, b))
  deriving (Show, Eq)

data ConDecl
  = ConDecl String [TypeExp]            -- Constructor name + field types
  deriving (Show, Eq)

data Decl
  = VarDef String Exp                   -- Variable declarations
  | FunDef String [String] Exp          -- Function declarations
  | TypeSig String TypeExp              -- Type annotations (e.g. f : a -> b)
  | DataDecl String [String] [ConDecl]  -- data Name vars = Con1 fields | Con2 fields
  deriving (Show, Eq)
