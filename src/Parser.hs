{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser(theParser) where

import Text.ParserCombinators.Parsec(parse, Parser, char, (<|>), eof, letter, chainl1, chainr1, alphaNum, digit, many1)
import Text.Parsec (Parsec, ParseError, try)
import Types
import Control.Monad (void)
import Util(Var)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Functor (($>))


theParser :: String -> Either ParseError TopLevel
theParser = parse (parseTopLevel <* eof) ""

lambdaCalculusDefinition =
  emptyDef {
    P.identStart = letter <|> char '_' ,
    P.identLetter = alphaNum <|> char '_' <|> char '\'',
    P.reservedNames = ["if", "then", "else", "bool", "true", "false", "nat", "succ", "pred", "iszero"],
    P.reservedOpNames = ["->", ".", ":", "λ", "\\", "="]
  }

lexer = P.makeTokenParser lambdaCalculusDefinition

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

parseVarName :: Parsec Var u Var
parseVarName = P.identifier lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

parseTopLevel :: Parser TopLevel
parseTopLevel = Right <$> try parseBinding <|> Left <$> parseTerm

parseBinding :: Parser Binding
parseBinding = do
  x <- parseVarName
  reservedOp "="
  t <- parseTerm
  return (x,t)

parseTerm :: Parser Term
parseTerm = chainl1 parseNonApp (return App)

parseNonApp :: Parser Term
parseNonApp = 
      parseConstB
  <|> parseConstN 
  <|> parseSucc
  <|> parsePred
  <|> parseIsZero
  <|> parseIfThenElse
  <|> parseAbs
  <|> parseVar
  <|> parens parseTerm 

parseIfThenElse :: Parser Term
parseIfThenElse = do
  reserved "if"
  t1 <- parseTerm
  reserved "then"
  t2 <- parseTerm
  reserved "else"
  IfThenElse t1 t2 <$> parseTerm

parseSucc :: Parser Term
parseSucc = reserved "succ" *> (Succ <$> parseTerm)

parsePred :: Parser Term
parsePred = reserved "pred" *> (Pred <$> parseTerm)

parseIsZero :: Parser Term
parseIsZero = reserved "iszero" *> (IsZero <$> parseTerm)

parseVar :: Parser Term
parseVar = Var <$> parseVarName

parseConstB :: Parser Term
parseConstB =
      reserved "true"  $> ConstB ConstTrue
  <|> reserved "false" $> ConstB ConstFalse

parseConstN :: Parser Term
parseConstN = ConstN . fromInteger <$> P.natural lexer

parseAbs :: Parser Term
parseAbs = do
    lambdaOp
    x <- parseVarName
    void $ reservedOp ":"
    t <- parseType
    void $ reservedOp "."
    Abs x t <$> parseTerm
  where
    lambdaOp :: Parser ()
    lambdaOp = void $ reservedOp "λ" <|> reservedOp "\\"

parseType :: Parser Type
parseType  = chainr1 parseNonArrow $ do
  reservedOp "->"
  return Arr

parseNonArrow :: Parser Type
parseNonArrow = parens parseType <|> parseBase <|> parseTVar

parseTVar :: Parser Type
parseTVar = TVar <$> parseVarName

parseBase :: Parser Type
parseBase = reserved "bool" $> Base BoolT <|> reserved "nat" $> Base NatT
