module Parser where

import Text.Parsec
import Control.Monad
import qualified Text.Parsec.Expr as E


type Parser a = Parsec String () a

data Parentheses = Parentheses Integer
                   deriving (Eq,Show)

data SingleAdd = SingleAdd Integer Integer
                 deriving (Eq,Show)

data Pattern = PVar String
               | PTuple [Pattern]
               deriving (Show, Eq)

data Expr = Num Integer
            | Var String
            | Parens Expr
            | PrefixOp String Expr
            | BinaryOp Expr String Expr
            | Tuple [Expr]
            | Call Expr [Expr]
            | App Expr Expr
            | Let Pattern Expr Expr
            | Defn String Pattern Expr
            deriving (Show, Eq)

reservedNames :: [String]
reservedNames = ["let", "in", "defn"]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x

integer :: Parser Integer
integer = read <$> lexeme (many1 digit)

identifier :: Parser String
identifier = lexeme (try (p >>= check))
  where
    p = (:) <$> firstChar <*> many nonFirstChar
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

    check x
      | x `elem` reservedNames =
        unexpected ("reserved word " ++ show x)
      | otherwise = return x

reserved :: String -> Parser ()
reserved w = lexeme . try $ do
    string w
    notFollowedBy (alphaNum <|> char '_')

symbol :: String -> Parser String
symbol s = lexeme $ string s

num :: Parser Expr
num = Num <$> integer

var :: Parser Expr
var = Var <$> identifier

parensOrTuple :: Parser Expr
parensOrTuple = between (symbol "(") (symbol ")") inner
  where
    inner = do
        es <- expr `sepBy1` symbol ","
        case es of
            []  -> return $ Tuple []
            [e] -> return $ Parens e
            ps  -> return $ Tuple ps

atom :: Parser Expr
atom = var <|> num <|> parensOrTuple

term :: Parser Expr
term = do
    f    <- atom
    args <- many atom
    return $ foldl App f args

pattern :: Parser Pattern
pattern = tuplePattern <|> simplePattern
  where
    simplePattern = PVar <$> identifier
    tuplePattern = do
        ps <- between (symbol "(") (symbol ")") (pattern `sepBy1` symbol ",")
        case ps of
            [p] -> return p
            ps' -> return $ PTuple ps'

table = [[prefix "-", prefix "+"]
        ,[binary "^" E.AssocLeft]
        ,[binary "*" E.AssocLeft
         ,binary "/" E.AssocLeft
         ,binary "%" E.AssocLeft]
        ,[binary "+" E.AssocLeft
         ,binary "-" E.AssocLeft]
        ,[binary "<" E.AssocNone
         ,binary ">" E.AssocNone]
        ,[binary "==" E.AssocRight]
        ,[prefix "not"]
        ,[binary "and" E.AssocLeft]
        ,[binary "or" E.AssocLeft]
        ]
  where
    binary name assoc =
        E.Infix (mkBinOp name <$ symbol name) assoc
    mkBinOp nm a b = BinaryOp a nm b
    prefix name = E.Prefix (PrefixOp name <$ symbol name)

logicExpr :: Parser Expr
logicExpr = E.buildExpressionParser table term

letExpr :: Parser Expr
letExpr = do
    reserved "let"
    p <- pattern
    symbol "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ Let p e1 e2

defnExpr :: Parser Expr
defnExpr = do
    reserved "defn"
    id <- identifier
    p <- pattern
    e <- expr
    return $ Defn id p e

expr :: Parser Expr
expr = defnExpr <|> letExpr <|> logicExpr

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where
    wrapper = do
        whitespace
        p
