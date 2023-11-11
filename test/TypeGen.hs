module TypeGen(genType) where

import Syntax (Type(..), BaseType (..))
import Hedgehog (Gen)
import Hedgehog.Gen (element, choice, recursive, subterm2)


genType :: Gen Type
genType =
  recursive choice
    [Base <$> genBase] 
    [ subterm2  genType genType PairT
    , subterm2  genType genType Arr
    ]

genBase :: Gen BaseType
genBase = element [BoolT, NatT, UnitT]