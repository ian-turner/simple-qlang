module QASMRender
  ( ClassicalType (..)
  , OutputLayout (..)
  , Stmt (..)
  , indent
  , renderClassicalType
  , renderOutputDecls
  , renderProgram
  , renderStmt
  , showOutputLayout
  ) where


data ClassicalType
  = CTypeInt
  | CTypeFloat
  | CTypeBool
  | CTypeBit
  deriving (Eq, Show)


data Stmt
  = StmtGate String [Int]
  | StmtMeasure String Int
  | StmtAssign String String
  | StmtDeclareAssign String String String
  | StmtIf String [Stmt]
  | StmtIfElse String [Stmt] [Stmt]
  | StmtSwitch String [(Int, [Stmt])]
  | StmtWhile String [Stmt]
  deriving (Eq, Show)


data OutputLayout
  = OutputScalars [ClassicalType]
  | OutputArray ClassicalType Int
  deriving (Eq)


showOutputLayout :: OutputLayout -> String
showOutputLayout (OutputScalars tys) =
  "scalars " ++ show tys
showOutputLayout (OutputArray ty n) =
  "array " ++ show ty ++ "[" ++ show n ++ "]"


renderClassicalType :: ClassicalType -> String
renderClassicalType CTypeInt   = "int[32]"
renderClassicalType CTypeFloat = "float[64]"
renderClassicalType CTypeBool  = "bool"
renderClassicalType CTypeBit   = "bit"


renderProgram :: Int -> OutputLayout -> [Stmt] -> String
renderProgram qubitCount outputLayout stmts =
  unlines $
    [ "OPENQASM 3.0;"
    , "include \"stdgates.inc\";"
    ]
      ++ qubitDecls
      ++ renderOutputDecls outputLayout
      ++ [""]
      ++ concatMap (renderStmt 0) stmts
  where
    qubitDecls
      | qubitCount <= 0 = []
      | otherwise       = ["qubit[" ++ show qubitCount ++ "] q;"]


renderOutputDecls :: OutputLayout -> [String]
renderOutputDecls (OutputScalars outputTypes) =
  [ renderClassicalType ty ++ " output_" ++ show i ++ ";"
  | (i, ty) <- zip [0 :: Int ..] outputTypes
  ]
renderOutputDecls (OutputArray CTypeBit n) =
  ["bit[" ++ show n ++ "] output;"]
renderOutputDecls (OutputArray ty n) =
  ["array[" ++ renderClassicalType ty ++ ", " ++ show n ++ "] output;"]


renderStmt :: Int -> Stmt -> [String]
renderStmt indentLevel (StmtGate gateName [q]) =
  [indent indentLevel ++ gateName ++ " q[" ++ show q ++ "];"]
renderStmt indentLevel (StmtGate gateName [q0, q1]) =
  [indent indentLevel ++ gateName ++ " q[" ++ show q0 ++ "], q[" ++ show q1 ++ "];"]
renderStmt indentLevel (StmtMeasure bitName q) =
  [indent indentLevel ++ "bit " ++ bitName ++ " = measure q[" ++ show q ++ "];"]
renderStmt indentLevel (StmtAssign name valueExpr) =
  [indent indentLevel ++ name ++ " = " ++ valueExpr ++ ";"]
renderStmt indentLevel (StmtDeclareAssign declType name valueExpr) =
  [indent indentLevel ++ declType ++ " " ++ name ++ " = " ++ valueExpr ++ ";"]
renderStmt indentLevel (StmtIf condition trueStmts) =
  [indent indentLevel ++ "if (" ++ condition ++ ") {"]
    ++ concatMap (renderStmt (indentLevel + 1)) trueStmts
    ++ [indent indentLevel ++ "}"]
renderStmt indentLevel (StmtIfElse condition trueStmts falseStmts) =
  [indent indentLevel ++ "if (" ++ condition ++ ") {"]
    ++ concatMap (renderStmt (indentLevel + 1)) trueStmts
    ++ [indent indentLevel ++ "} else {"]
    ++ concatMap (renderStmt (indentLevel + 1)) falseStmts
    ++ [indent indentLevel ++ "}"]
renderStmt indentLevel (StmtWhile cond body) =
  [indent indentLevel ++ "while (" ++ cond ++ ") {"]
    ++ concatMap (renderStmt (indentLevel + 1)) body
    ++ [indent indentLevel ++ "}"]
renderStmt indentLevel (StmtSwitch scrutinee arms) =
  [indent indentLevel ++ "switch (" ++ scrutinee ++ ") {"]
    ++ concatMap renderArm arms
    ++ [indent indentLevel ++ "}"]
  where
    renderArm (tag, armStmts) =
      [indent (indentLevel + 1) ++ "case " ++ show tag ++ ":"]
        ++ [indent (indentLevel + 2) ++ "{"]
        ++ concatMap (renderStmt (indentLevel + 3)) armStmts
        ++ [indent (indentLevel + 2) ++ "}"]
renderStmt _ (StmtGate _ qs) =
  error ("unsupported gate statement arity: " ++ show (length qs))


indent :: Int -> String
indent n =
  replicate (n * 2) ' '
