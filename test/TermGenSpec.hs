{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TermGenSpec(termGen_props) where
import TermGen(genTermTypePair)
import Hedgehog (forAll, Property, (===), property)
import Typecheck (typecheckNoContext)

termGen_props :: [Property]
termGen_props = [genTerm_is_welltyped]

genTerm_is_welltyped :: Property
genTerm_is_welltyped = property $ do
  (t,ty) <- forAll genTermTypePair
  typecheckNoContext t === Right ty