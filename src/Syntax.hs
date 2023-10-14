{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Syntax where

type Var = String

data Type =
    Arr Type Type 
  | Base BaseType
  deriving (Read, Show)

data BaseType = BoolT | NatT | UnitT
  deriving (Read, Show)

data Term =
    Abs Pattern Type Term
  | App Term Term
  | Seq Term Term
  | Var Var
  | ConstB ConstBool
  | ConstN ConstNat
  | IfThenElse Term Term Term
  | Pred Term
  | Succ Term
  | IsZero Term
  | Unit
  deriving (Read, Show)

type ConstNat = Int

data Pattern = Identifier Var | Wildcard
  deriving (Read, Show)

data ConstBool = ConstTrue | ConstFalse
  deriving (Read, Show)

type Binding = (Var, Term)

type TopLevel = Either Term Binding

magnitude :: ConstNat -> Int
magnitude = id