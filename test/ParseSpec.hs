{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module ParseSpec(type_props, term_props, parse_props) where

import Hedgehog (forAll, Property, property, tripping, Gen)
import Hedgehog.Gen (small)
import Text.Parsec.String (Parser)

import Parser(parseType, stringParse, parseTerm0, parseTopLevel)
import PPrint(pshow, PPrint)
import TermGen(genTerm)
import TypeGen(genType)
import Syntax (Binding(TermBind), TopLevel (TopBind))

prop_parse_pshow_id :: (Eq a, Show a, PPrint a) => Gen a -> Parser a -> Property
prop_parse_pshow_id g p = property $ do
  t <- forAll g
  tripping t pshow (stringParse p)

prop_pshow_parse_id_type :: Property
prop_pshow_parse_id_type = prop_parse_pshow_id genType parseType

prop_pshow_parse_id_term :: Property
prop_pshow_parse_id_term = prop_parse_pshow_id genSmallTerm parseTerm0
  where genSmallTerm =  iterate small genTerm !! 2

prop_parse_topBind :: Property
prop_parse_topBind = prop_parse_pshow_id genBind parseTopLevel
  where
    genBind      = TopBind . TermBind "x" <$> genSmallTerm
    genSmallTerm = iterate small genTerm !! 3

type_props :: [Property]
type_props = [prop_pshow_parse_id_type]


term_props :: [Property]
term_props = [prop_pshow_parse_id_term]

parse_props :: [Property]
parse_props = [prop_pshow_parse_id_type, prop_pshow_parse_id_term, prop_parse_topBind]