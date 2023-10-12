{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Types where

type Var = String

data Type =
    Arr Type Type 
  | TVar Var
  | Base BaseType
  deriving (Read, Show)

data BaseType = BoolT | NatT | UnitT
  deriving (Read, Show)

data Term =
    Abs Var Type Term
  | App Term Term
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

data ConstBool = ConstTrue | ConstFalse
  deriving (Read, Show)

type Binding = (Var, Term)

type TopLevel = Either Term Binding

magnitude :: ConstNat -> Int
magnitude = id