module TestContext(
    Context
  , typeLookup
  , insert
  , isEmpty
  , empty
  , toList
  , typeEq
  ) where

import Syntax
import qualified Typecheck(typeEq)
import qualified Context(empty)

typeEq :: Type -> Type -> Bool
typeEq = Typecheck.typeEq Context.empty


type Context = [(Var, Type)]

typeLookup :: Type -> Context -> [Var]
typeLookup t = map fst . filter (typeEq t . snd)

insert :: (Var, Type) -> Context -> Context
insert = (:)

isEmpty :: Context -> Bool
isEmpty = null

empty :: Context
empty = []

toList :: Context -> [(Var, Type)]
toList = id