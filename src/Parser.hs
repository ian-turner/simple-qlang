module Parser where

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap)
import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing


type Parser a = Parsec String () a

data Parentheses = Parentheses Integer
                   deriving (Eq,Show)

data SingleAdd = SingleAdd Integer Integer
                 deriving (Eq,Show)

data Expr = Num Integer
            | Var String
            | Add Expr Expr
            | Parens Expr
            deriving (Show, Eq)

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x

num :: Parser Expr
num = do
    n <- lexeme $ many1 digit
    return $ Num $ read n

var :: Parser Expr
var = lexeme $ do
    fc <- firstChar
    rest <- many nonFirstChar
    return $ Var (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parens :: Parser Expr -> Parser Expr
parens exprImpl = do
    void $ lexeme $ char '('
    e <- exprImpl
    void $ lexeme $ char ')'
    return $ Parens e

add :: Parser Expr
add = do
    e0 <- expr
    void $ lexeme $ char '+'
    e1 <- expr
    return $ Add e0 e1

numOrVar :: Parser Expr
numOrVar = num <|> var

term :: Parser Expr -> Parser Expr
term exprImpl = num <|> var <|> parens exprImpl

expr :: Parser Expr
expr = chainl1 termP op
  where
    op = do
        void $ lexeme $ char '+'
        return Add
    termP = term expr

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where
    wrapper = do
        whitespace
        p

parseResult = parseWithWhitespace expr "32"
