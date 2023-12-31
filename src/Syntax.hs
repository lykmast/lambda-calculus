{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Syntax where

type Var = String

data Type =
    Arr Type Type 
  | Base BaseType
  | Alias String
  | PairT Type Type
  deriving (Eq, Read, Show)

data BaseType = BoolT | NatT | UnitT
  deriving (Eq, Read, Show)

data Term =
    Abs Pattern Type Term
  | App Term Term
  | Let Pattern Term Term
  | Seq Term Term
  | As  Term Type
  | Var Var
  | ConstB ConstBool
  | ConstN ConstNat
  | Pair Term Term
  | Proj1 Term
  | Proj2 Term
  | IfThenElse Term Term Term
  | Pred Term
  | Succ Term
  | IsZero Term
  | Unit
  deriving (Eq, Read, Show)

type ConstNat = Int

data Pattern = Identifier Var | Wildcard
  deriving (Eq, Read, Show)

data ConstBool = ConstTrue | ConstFalse
  deriving (Eq, Read, Show)

data Binding = TermBind Var Term | TypeBind Var Type
  deriving (Eq, Read, Show)

data TopLevel = TopTerm Term | TopBind Binding
  deriving (Eq, Read, Show)

magnitude :: ConstNat -> Int
magnitude = id