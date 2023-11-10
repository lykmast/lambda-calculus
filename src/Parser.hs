{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser(theParser) where

import Text.ParserCombinators.Parsec(parse, Parser, char, (<|>), eof, letter, chainl1, chainr1, alphaNum)
import Text.Parsec (Parsec, ParseError, try)
import Syntax
import Control.Monad (void)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Functor (($>))
import Data.Char (isUpper)


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
    P.reservedOpNames = ["->", ".", ":", "λ", "\\", "=", ".1", ".2"]
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
parseTerm0 = chainl1 parseTerm1 (reservedOp ";" $> Seq)

parseTerm1 :: Parser Term
parseTerm1 = try parseAs <|> parseTerm2

parseAs :: Parser Term
parseAs = As <$> parseTerm2 <* reserved "as" <*> parseType

parseTerm2 :: Parser Term
parseTerm2 = chainl1 parseNonApp (return App)

parseNonApp :: Parser Term
parseNonApp =
      parseConstB
  <|> parseConstN
  <|> parseUnit
  <|> parseSucc
  <|> parsePred
  <|> parseIsZero
  <|> parseIfThenElse
  <|> parseLet
  <|> parseAbs
  <|> parseVar
  <|> parens parseTerm0

parseIfThenElse :: Parser Term
parseIfThenElse = do
  reserved "if"
  t1 <- parseTerm1
  reserved "then"
  t2 <- parseTerm1
  reserved "else"
  IfThenElse t1 t2 <$> parseTerm1

parseLet :: Parser Term
parseLet = do
  reserved "let"
  p <- parsePattern
  reservedOp "="
  t1 <- parseTerm1
  reserved "in"

parsePair :: Parser Term
parsePair = uncurry Pair <$> braces parsePairTerms

parsePairTerms :: Parser (Term, Term)
parsePairTerms = (,) <$> parseTerm0 <* comma <*> parseTerm0


parseSucc :: Parser Term
parseSucc = reserved "succ" *> (Succ <$> parseTerm2)

parsePred :: Parser Term
parsePred = reserved "pred" *> (Pred <$> parseTerm2)

parseIsZero :: Parser Term
parseIsZero = reserved "iszero" *> (IsZero <$> parseTerm2)

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
    Abs x t <$> parseTerm1
  where
    lambdaOp :: Parser ()
    lambdaOp = void $ reservedOp "λ" <|> reservedOp "\\"

parseType :: Parser Type
parseType  = chainr1 parseNonArrow $ do
  reservedOp "->"
  return Arr

parseNonArrow :: Parser Type
parseNonArrow = parens parseType <|> parseBase <|> parseAlias

parseAlias :: Parser Type
parseAlias = Alias <$> parseTypeName

parseBase :: Parser Type
parseBase =
      reserved "Bool" $> Base BoolT
  <|> reserved "Nat"  $> Base NatT
  <|> reserved "Unit" $> Base UnitT
