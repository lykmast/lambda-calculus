{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser(theParser, parseTopLevel, parseType, parseTerm0, stringParse) where

import Text.ParserCombinators.Parsec(parse, Parser, char, (<|>), eof, letter, chainl1, chainr1, alphaNum, string, optional)
import Text.Parsec (Parsec, ParseError, try)
import Syntax
import Control.Monad (void)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Functor (($>))
import Data.Char (isUpper)
import Control.Applicative ((<**>))
import Text.Parsec.Token (GenTokenParser(whiteSpace))

stringParse :: Parser a -> String -> Either ParseError a
stringParse p = parse (p <* eof) ""

theParser :: String -> Either ParseError TopLevel
theParser = parse (parseTopLevel <* eof) ""

lambdaCalculusDefinition =
  emptyDef {
    P.identStart = letter <|> char '_' ,
    P.identLetter = alphaNum <|> char '_' <|> char '\'',
    P.reservedNames = [
        "if"
      , "then"
      , "else"
      , "let"
      , "in"
      , "true"
      , "false"
      , "succ"
      , "pred"
      , "iszero"
      , "unit"
      , "Bool"
      , "Nat"
      , "Unit"
      , "as"
      , "_"],
    P.reservedOpNames = ["->", ".", ":", "λ", "\\", "=", ".1", ".2", "\215"]
  }

lexer = P.makeTokenParser lambdaCalculusDefinition

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

parseVarName :: Parsec Var u Var
parseVarName = P.identifier lexer

parseTypeName :: Parsec Var u Var
parseTypeName = do
  s <- parseVarName
  if isUpper (head s) then return s else fail "Type name must start with uppercase letter."

parens :: Parser a -> Parser a
parens = P.parens lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

comma :: Parser ()
comma = void (P.comma lexer)

parseTopLevel :: Parser TopLevel
parseTopLevel = TopBind <$> try parseBinding <|> TopTerm <$> parseTerm0

parseBinding :: Parser Binding
parseBinding = try parseTermBind <|> parseTypeBind

parseTermBind :: Parser Binding
parseTermBind = TermBind <$> parseVarName <* reservedOp "=" <*> parseTerm0

parseTypeBind :: Parser Binding
parseTypeBind =
  -- reserved "type" $> 
    TypeBind <$>
        parseTypeName
    <* reservedOp "="
    <*> parseType

parseTerm0 :: Parser Term
parseTerm0 = chainl1 parseNonSeq (reservedOp ";" $> Seq)

parseAbsSubterm :: Parser Term
parseAbsSubterm = parseNonSeq

parseNonSeq :: Parser Term
parseNonSeq = try parseAs <|> parseAbslikeTerm
  where 
    parseAs :: Parser Term
    parseAs = As <$> parseAbslikeTerm <* reserved "as" <*> parseType

parseAbslikeTerm :: Parser Term
parseAbslikeTerm =
      parseLet
  <|> parseIfThenElse
  <|> parseAbs
  <|> parseApplikeTerm

parseApplikeTerm :: Parser Term
parseApplikeTerm =
      try parseSucc
  <|> try parsePred
  <|> try parseIsZero
  <|> chainl1 parseAppSubterm (return App)

parsePostfixLeft :: Parser Term -> Parser (Term -> Term) -> Parser Term
parsePostfixLeft p op = p <**> go 
  where go = (flip (.) <$> op) <*> (go <|> return id)

parseAppSubterm :: Parser Term
parseAppSubterm =
  try (parseAtomTerm `parsePostfixLeft` parseProjOp)
  <|> parseAtomTerm

parseAtomTerm :: Parser Term
parseAtomTerm = 
      parseConstB
  <|> parseConstN
  <|> parseUnit
  <|> parseVar
  <|> parsePair
  <|> parens parseTerm0

parseIfThenElse :: Parser Term
parseIfThenElse = do
  reserved "if"
  t1 <- parseAbsSubterm
  reserved "then"
  t2 <- parseAbsSubterm
  reserved "else"
  IfThenElse t1 t2 <$> parseAbsSubterm

parseLet :: Parser Term
parseLet = do
  reserved "let"
  p <- parsePattern
  reservedOp "="
  t1 <- parseAbsSubterm
  reserved "in"
  Let p t1 <$> parseAbsSubterm

parsePair :: Parser Term
parsePair = uncurry Pair <$> braces parsePairTerms

parsePairTerms :: Parser (Term, Term)
parsePairTerms = (,) <$> parseTerm0 <* comma <*> parseTerm0

parseProjOp :: Parser (Term -> Term)
parseProjOp = tokenize $ 
    try (Proj1 <$ string ".1") <|>  Proj2 <$ string ".2"
  where
    tokenize p    = optWhitespace *> p <* optWhitespace 
    optWhitespace = optional (whiteSpace lexer)

parseSucc :: Parser Term
parseSucc = reserved "succ" *> (Succ <$> parseAppSubterm)

parsePred :: Parser Term
parsePred = reserved "pred" *> (Pred <$> parseAppSubterm)

parseIsZero :: Parser Term
parseIsZero = reserved "iszero" *> (IsZero <$> parseAppSubterm)

parseVar :: Parser Term
parseVar = Var <$> parseVarName

parseConstB :: Parser Term
parseConstB =
      reserved "true"  $> ConstB ConstTrue
  <|> reserved "false" $> ConstB ConstFalse

parseUnit :: Parser Term
parseUnit = reserved "unit" $> Unit

parseConstN :: Parser Term
parseConstN = ConstN . fromInteger <$> P.natural lexer

parsePattern :: Parser Pattern
parsePattern = Wildcard <$ reserved "_" <|> Identifier <$> parseVarName

parseAbs :: Parser Term
parseAbs = do
    lambdaOp
    x <- parsePattern
    void $ reservedOp ":"
    t <- parseType
    void $ reservedOp "."
    Abs x t <$> parseAbsSubterm
  where
    lambdaOp :: Parser ()
    lambdaOp = void $ reservedOp "λ" <|> reservedOp "\\"

parseType :: Parser Type
parseType  = chainr1 parseType1 $ do
  reservedOp "->"
  return Arr

parseType1 :: Parser Type
parseType1 = chainl1 parseType2 $ do
  parsePairX
  return PairT

parseType2 :: Parser Type
parseType2 = parens parseType <|> parseBase <|> parseAlias <|> parsePairT

parseAlias :: Parser Type
parseAlias = Alias <$> parseTypeName

parseBase :: Parser Type
parseBase =
      reserved "Bool" $> Base BoolT
  <|> reserved "Nat"  $> Base NatT
  <|> reserved "Unit" $> Base UnitT


parsePairX :: Parser ()
parsePairX = reservedOp "\215"

parsePairT :: Parser Type
parsePairT =
      uncurry PairT <$> braces ((,) <$> parseType <* comma <*> parseType)
  <|> PairT <$> parseType <* parsePairX <*> parseType