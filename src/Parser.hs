module Parser where

import Text.Parsec
import Control.Monad
import qualified Text.Parsec.Expr as E

import ConcreteSyntax


type Parser a = Parsec String () a

reservedNames :: [String]
reservedNames = ["let", "in", "if", "then", "else"]

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

num :: Parser Exp
num = Num <$> integer

var :: Parser Exp
var = Var <$> identifier

unitParensOrTuple :: Parser Exp
unitParensOrTuple = unitPattern <|> (between (symbol "(") (symbol ")") inner)
  where
    unitPattern = do
        reserved "()"
        return Unit
    inner = do
        es <- expr `sepBy1` symbol ","
        case es of
            [e] -> return e
            ps  -> return $ Tuple ps

atom :: Parser Exp
atom = var <|> num <|> unitParensOrTuple

term :: Parser Exp
term = do
    f    <- atom
    args <- many atom
    return $ foldl App f args

pattern :: Parser [String]
pattern = tuplePattern <|> simplePattern
  where
    simplePattern = do
        id <- identifier
        return [id]
    tuplePattern = do
        ps <- between (symbol "(") (symbol ")") (identifier `sepBy1` symbol ",")
        return ps

letExpr :: Parser Exp
letExpr = do
    reserved "let"
    p <- pattern
    symbol "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ Let p e1 e2

lamExpr :: Parser Exp
lamExpr = do
    reserved "\\"
    p <- pattern
    symbol "->"
    e <- expr
    return $ Lam p e

ifElseExpr :: Parser Exp
ifElseExpr = do
    reserved "if"
    eBool <- expr
    reserved "then"
    trueExp <- expr
    reserved "else"
    falseExp <- expr
    return $ IfExp eBool trueExp falseExp

arrowExpr :: Parser Exp
arrowExpr = do
    e1 <- expr
    symbol "->"
    e2 <- expr
    return $ Arrow e1 e2

expr :: Parser Exp
expr = lamExpr <|> letExpr <|> ifElseExpr <|> term

typeDecl :: Parser Decl
typeDecl = do
    id <- identifier
    symbol ":"
    ts <- expr `sepBy1` (symbol "->")
    case ts of
        [t] -> return $ TypeDecl id t
        (t:ts) ->  return $ TypeDecl id (foldr Arrow t ts)

varOrFunDecl :: Parser Decl
varOrFunDecl = do
    ids <- many identifier
    symbol "="
    e <- expr
    case ids of
        [id] -> return $ VarDecl id e
        (funId : vars) -> return $ FunDecl funId vars e

decls :: Parser Decl
decls = typeDecl <|> varOrFunDecl

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where
    wrapper = do
        whitespace
        p
