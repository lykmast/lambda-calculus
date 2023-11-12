-- {-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module ParseSpec(type_props, term_props, parse_props) where

import Parser(parseType, stringParse, parseTerm0, theParser)
import PPrint(pshow)
import TermGen(genTerm)
import TypeGen(genType)
import qualified Hedgehog as H(forAll, Property, (===), property)
import Hedgehog.Gen (small)
import Syntax (Binding(TermBind), TopLevel (TopBind))


prop_pshow_parse_id_type :: H.Property
prop_pshow_parse_id_type = H.property $ do
  ty <- H.forAll genType 
  stringParse parseType (pshow ty) H.=== Right ty

prop_pshow_parse_id_term :: H.Property
prop_pshow_parse_id_term = H.property $ do
  t <- H.forAll (iterate small genTerm !! 2)
  stringParse parseTerm0 (pshow t) H.=== Right t

prop_parse_topBind :: H.Property
prop_parse_topBind = H.property $ do
  t <- H.forAll (iterate small genTerm !! 3)
  let termBind = TopBind (TermBind "x" t)
  theParser ("x = " ++ pshow t) H.=== Right termBind

type_props :: [H.Property]
type_props = [prop_pshow_parse_id_type]


term_props :: [H.Property]
term_props = [prop_pshow_parse_id_term]

parse_props :: [H.Property]
parse_props = [prop_pshow_parse_id_type, prop_pshow_parse_id_term, prop_parse_topBind]