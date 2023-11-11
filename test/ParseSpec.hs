module ParseSpec(type_props, term_props) where

import Parser(parseType, stringParse, parseTerm0)
import PPrint(pshow)
import TermGen(genTerm)
import TypeGen(genType)
import qualified Hedgehog as H(forAll, Property, (===), property)
import Hedgehog.Gen (small)


prop_pshow_parse_id_type :: H.Property
prop_pshow_parse_id_type = H.property $ do
  ty <- H.forAll genType 
  stringParse parseType (pshow ty) H.=== Right ty

prop_pshow_parse_id_term :: H.Property
prop_pshow_parse_id_term = H.property $ do
  t <- H.forAll (iterate small genTerm !! 2)
  stringParse parseTerm0 (pshow t) H.=== Right t


type_props :: [H.Property]
type_props = [prop_pshow_parse_id_type]


term_props :: [H.Property]
term_props = [prop_pshow_parse_id_term]