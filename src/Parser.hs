{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Monad
import Control.Monad.Identity
import Text.Parsec.Indent
import Text.Parsec.Language
import Text.Parsec hiding (Empty, ParseError, State)
import qualified Data.IntMap as IM
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Expr as E

import ConcreteSyntax


type Parser a = IndentParser String ParserState a

data ParserState = 
  ParserState
    { expOpTable :: IM.IntMap [E.Operator String ParserState (IndentT Identity) Exp]
    }

initialParserState :: ParserState
initialParserState =
  ParserState
    { expOpTable = IM.fromAscList (zip [0 ..] initialOpTable)
    }

initialOpTable :: [[E.Operator String ParserState (IndentT Identity) Exp]]
initialOpTable = [ [] ]

parseModule :: String -> String -> ParserState
               -> Either P.ParseError ([Decl], ParserState)
parseModule srcName cnts st = runIndent $ runParserT decls st srcName cnts

decls :: Parser ([Decl], ParserState)
decls = do
    bs <- block funDef
    st <- getState
    return (bs, st)

funDef :: Parser Decl
funDef = do
  f <- var
  args <- many var
  reservedOp "="
  return $ FunDef f args Unit

var :: Parser String
var = do
  name <- identifier
  return name

-- | Language token definition
langStyle :: (Stream s m Char, Monad m) => Token.GenLanguageDef s u m
langStyle =
  Token.LanguageDef
    { Token.commentStart = "{-"
    , Token.commentEnd = "-}"
    , Token.commentLine = "--"
    , Token.nestedComments = True
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> oneOf "_'"
    , Token.opStart = oneOf "!&*+/"
    , Token.opLetter = oneOf "!&*+/"
    , Token.caseSensitive = True
    , Token.reservedNames = 
      [ "in"
      , "let"
      , "if"
      , "then"
      , "else"
      ]
    , Token.reservedOpNames =
        [ "\\"
        ]
    }

-- | Parse a token
tokenizer :: (Stream s m Char, Monad m) => Token.GenTokenParser s u m
tokenizer = Token.makeTokenParser langStyle

-- | Parse a literal
stringLiteral :: (Stream s m Char, Monad m) => ParsecT s u m String
stringLiteral = Token.stringLiteral tokenizer

-- | Parse an legal identifier
identifier :: (Stream s m Char, Monad m) => ParsecT s u m String
identifier = Token.identifier tokenizer

-- | Parse many white spaces
whiteSpace :: (Stream s m Char, Monad m) => ParsecT s u m ()
whiteSpace = Token.whiteSpace tokenizer

-- | Parse a reserved word
reserved :: (Stream s m Char, Monad m) => String -> ParsecT s u m ()
reserved = Token.reserved tokenizer

-- | Parse a reserved operator
reservedOp :: (Stream s m Char, Monad m) => String -> ParsecT s u m ()
reservedOp = Token.reservedOp tokenizer

-- | Parse an integer
integer :: (Stream s m Char, Monad m) => ParsecT s u m Integer
integer = Token.integer tokenizer

-- | Parse an natural number
naturals :: (Stream s m Char, Monad m) => ParsecT s u m Integer
naturals = Token.natural tokenizer

-- | Parse an operator
operator :: (Stream s m Char, Monad m) => ParsecT s u m String
operator = Token.operator tokenizer

-- | Parse a pair of parenthesis
parens :: (Stream s m Char, Monad m) => ParsecT s u m a -> ParsecT s u m a
parens = Token.parens tokenizer

-- | Parse a comma
comma :: (Stream s m Char, Monad m) => ParsecT s u m String
comma = Token.comma tokenizer

lexeme :: (Stream s m Char, Monad m) => ParsecT s u m a -> ParsecT s u m a
lexeme = Token.lexeme tokenizer
