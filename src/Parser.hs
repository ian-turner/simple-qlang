{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Control.Monad
import Control.Monad.Identity
import Data.Char (isUpper, isLower)
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
    { expParser :: IndentParser String ParserState Exp
    , expOpTable :: IM.IntMap [E.Operator String ParserState (IndentT Identity) Exp]
    }

initialParserState :: ParserState
initialParserState =
  ParserState
    { expParser = E.buildExpressionParser initialOpTable atomExp
    , expOpTable = IM.fromAscList (zip [0 ..] initialOpTable)
    }

initialOpTable :: [[E.Operator String ParserState (IndentT Identity) Exp]]
initialOpTable =
  [ [ E.Infix (reservedOp "||" >> return (BinOp "||")) E.AssocLeft ]
  , [ E.Infix (reservedOp "&&" >> return (BinOp "&&")) E.AssocLeft ]
  , [ E.Infix (reservedOp "==" >> return (BinOp "==")) E.AssocNone
    , E.Infix (reservedOp "/=" >> return (BinOp "/=")) E.AssocNone
    , E.Infix (reservedOp "<"  >> return (BinOp "<"))  E.AssocNone
    , E.Infix (reservedOp ">"  >> return (BinOp ">"))  E.AssocNone
    , E.Infix (reservedOp "<=" >> return (BinOp "<=")) E.AssocNone
    , E.Infix (reservedOp ">=" >> return (BinOp ">=")) E.AssocNone
    ]
  , [ E.Infix (reservedOp "+" >> return (BinOp "+")) E.AssocLeft
    , E.Infix (reservedOp "-" >> return (BinOp "-")) E.AssocLeft
    ]
  , [ E.Infix (reservedOp "*" >> return (BinOp "*")) E.AssocLeft
    , E.Infix (reservedOp "/" >> return (BinOp "/")) E.AssocLeft
    ]
  ]

parseModule :: String -> String -> ParserState
               -> Either P.ParseError ([Decl], ParserState)
parseModule srcName cnts st = runIndent $ runParserT decls st srcName cnts

decls :: Parser ([Decl], ParserState)
decls = do
    reserved "module"
    name <- identifier
    reserved "where"
    bs <- block decl
    st <- getState
    return (bs, st)

decl :: Parser Decl
decl = try dataDecl <|> try typeSig <|> try funDef <|> varDef

funDef :: Parser Decl
funDef = do
  f <- var
  args <- many1 var
  reservedOp "="
  def <- term
  return $ FunDef f args def

varDef :: Parser Decl
varDef = do
  name <- var
  reservedOp "="
  def <- term
  return $ VarDef name def

term :: Parser Exp
term = getState >>= \st -> expParser st

atomExp :: Parser Exp
atomExp =
  unit
  <|> try boolLit
  <|> try strLit
  <|> try piLit
  <|> try lamExp
  <|> try letExp
  <|> try caseExp
  <|> try tupleExp
  <|> try ifExp
  <|> try negNum
  <|> try num
  <|> appExp
  <|> varExp

unit :: Parser Exp
unit = reservedOp "()" >> return Unit

num :: Parser Exp
num = try numFloat <|> try piLit <|> numInt
  where
    numInt = do
      d <- lexeme (many1 digit)
      return $ NumInt (read d)
    numFloat = do
      l <- lexeme (many1 digit)
      reservedOp "."
      r <- lexeme (many1 digit)
      let numStr = l ++ "." ++ r
      return $ NumFloat numStr

negNum :: Parser Exp
negNum = do
  reservedOp "-"
  n <- num
  case n of
    NumInt x   -> return $ NumInt (negate x)
    NumFloat x -> return $ NumFloat ("-" ++ x)
    _          -> fail "expected number after -"

appExp :: Parser Exp
appExp =
  manyLines
      (do headE <- headExp
          return $ foldl (\z x -> App z x) headE)
  arg
  where
    headExp =
      dynliftExp <|> varExp
    arg =
      try unit <|> try boolLit <|> try strLit <|> try piLit <|> try num <|> dynliftExp <|> varExp <|> tupleExp

boolLit :: Parser Exp
boolLit = (reserved "True" >> return (BoolLit True))
       <|> (reserved "False" >> return (BoolLit False))

strLit :: Parser Exp
strLit = StringLit <$> stringLiteral

piLit :: Parser Exp
piLit = reserved "pi" >> return (NumFloat "pi")

tupleExp :: Parser Exp
tupleExp = do
  tms <- parens (term `sepBy1` comma)
  case tms of
    []  -> return Unit
    [t] -> return t
    ts  -> return $ Tuple ts

letExp :: Parser Exp
letExp = do
  reserved "let"
  bs <- block bind
  reserved "in"
  t <- term
  return $ Let bs t
  where
    bind = try tuple <|> single
    single = do
      n <- try var
      reservedOp "="
      d <- term
      return $ BSingle n d
    tuple = do
      ns <- parens $ try $ var `sepBy1` comma
      reservedOp "="
      d <- term
      return $ BTuple ns d

ifExp :: Parser Exp
ifExp = do
  reserved "if"
  b <- term
  reserved "then"
  t1 <- term
  reserved "else"
  t2 <- term
  return $ IfExp b t1 t2

lamExp :: Parser Exp
lamExp = do
  reservedOp "\\"
  args <- many1 var
  reservedOp "->"
  e <- term
  return $ Lam args e

dynliftExp :: Parser Exp
dynliftExp = reserved "dynlift" >> return Dynlift

-- | Parse a case expression
caseExp :: Parser Exp
caseExp = do
  reserved "case"
  scrut <- term
  reserved "of"
  alts <- block caseAlt
  return $ CaseExp scrut alts

-- | Parse a single case alternative: pat -> expr
caseAlt :: Parser (Pat, Exp)
caseAlt = do
  p <- pat
  reservedOp "->"
  e <- term
  return (p, e)

-- | Parse a flat pattern
pat :: Parser Pat
pat = try patCon <|> try patTuple <|> patUnit <|> patWild <|> patVar
  where
    patVar = do
      name <- identifier
      return $ PVar name
    patWild =
      lexeme (char '_' <* notFollowedBy (alphaNum <|> oneOf "_'")) >> return PWild
    patUnit =
      reservedOp "()" >> return PUnit
    patTuple = do
      args <- parens (flatArg `sepBy1` comma)
      case args of
        [FArg x] -> return $ PVar x
        [FWild]  -> return PWild
        _        -> return $ PTuple args
    patCon = do
      name <- conName
      args <- many flatArg
      return $ PCon name args

-- | Parse a flat pattern argument (variable or wildcard)
flatArg :: Parser FlatArg
flatArg = flatWild <|> flatVar
  where
    flatWild =
      lexeme (char '_' <* notFollowedBy (alphaNum <|> oneOf "_'")) >> return FWild
    flatVar = do
      name <- identifier
      return $ FArg name

-- | Parse a data type declaration: data Name vars = Con1 fields | Con2 fields
dataDecl :: Parser Decl
dataDecl = do
  reserved "data"
  name <- conName
  vars <- many tyVarName
  reservedOp "="
  cons <- conDecl `sepBy1` reservedOp "|"
  return $ DataDecl name vars cons

-- | Parse a single constructor declaration: ConName field1 field2 ...
conDecl :: Parser ConDecl
conDecl = withPos $ do
  name <- conName
  fields <- many (try (indented >> typeAtom))
  return $ ConDecl name fields

-- | Parse a constructor name (uppercase identifier)
conName :: Parser String
conName = try $ do
  name <- identifier
  if isUpper (head name)
    then return name
    else fail "expected constructor name"

-- | Parse a type variable name (lowercase identifier)
tyVarName :: Parser String
tyVarName = try $ do
  name <- identifier
  if isLower (head name)
    then return name
    else fail "expected type variable"

-- | Parse a type annotation declaration: name : typeExp
typeSig :: Parser Decl
typeSig = do
  name <- var
  reservedOp ":"
  ty <- typeExp
  return $ TypeSig name ty

-- | Parse a type expression (right-associative ->)
typeExp :: Parser TypeExp
typeExp = do
  t <- typeApp
  option t $ do
    reservedOp "->"
    t' <- typeExp
    return $ TyFun t t'

-- | Parse type application (juxtaposition, left-associative).
-- Uses withPos/indented so subsequent atoms must be indented further than
-- the start of the application, preventing consumption of the next declaration.
typeApp :: Parser TypeExp
typeApp = withPos $ do
  t <- typeAtom
  ts <- many (try (indented >> typeAtom))
  return $ foldl TyApp t ts

-- | Parse an atomic type expression
typeAtom :: Parser TypeExp
typeAtom = tyVar <|> tyCon <|> tyParens

-- | Parse a type variable (lowercase identifier)
tyVar :: Parser TypeExp
tyVar = try $ do
  name <- identifier
  if isLower (head name)
    then return (TyVar name)
    else fail "expected type variable"

-- | Parse a type constructor (uppercase identifier)
tyCon :: Parser TypeExp
tyCon = try $ do
  name <- identifier
  if isUpper (head name)
    then return (TyCon name)
    else fail "expected type constructor"

-- | Parse a parenthesized type or tuple type
tyParens :: Parser TypeExp
tyParens = do
  ts <- parens (typeExp `sepBy1` comma)
  case ts of
    [t] -> return t
    _   -> return $ TyTuple ts

varExp :: Parser Exp
varExp = (var >>= \x -> return $ Var x)

var :: Parser String
var = do
  name <- identifier
  return name

manyLines :: Parser ([a] -> b) -> Parser a -> Parser b
manyLines h p = withPos $ h <*/> p

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
    , Token.opStart = oneOf "!&*+/=-:|<>"
    , Token.opLetter = oneOf "!&*+/=-:|<>"
    , Token.caseSensitive = True
    , Token.reservedNames =
      [ "in"
      , "let"
      , "if"
      , "then"
      , "else"
      , "dynlift"
      , "data"
      , "where"
      , "module"
      , "case"
      , "of"
      , "True"
      , "False"
      , "pi"
      ]
    , Token.reservedOpNames =
        [ "\\"
        , "="
        , "()"
        , "->"
        , ":"
        , "|"
        , "+"
        , "-"
        , "*"
        , "/"
        , "=="
        , "/="
        , "<"
        , ">"
        , "<="
        , ">="
        , "&&"
        , "||"
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
