module TermGen(genTerm, genTermTypePair) where

import Hedgehog ( Gen )
import Hedgehog.Gen (element, int, choice, subterm, subterm2, subterm3, recursive, small, prune)
import qualified Hedgehog.Range as Range(constant)

import Syntax (Term(..), ConstBool(..), Pattern (..), Type (..), BaseType (..))
import qualified TypeGen as T(genType)
import TestContext (isEmpty, Context, toList, insert, typeLookup, empty)

import Data.Bifunctor (first)

genType :: Gen Type
genType = iterate small T.genType !! 3

genTerm :: Gen Term
genTerm = fst <$> genTermTypePair

genTermTypePair :: Gen (Term, Type)
genTermTypePair = genFromContext empty

genFromContext :: Context -> Gen (Term, Type)
genFromContext ctx = do
  ty <- genType
  let gNoVar = (,) <$> genTermWithType ctx ty <*> return ty 
  case genWtVar ctx of
    Nothing   -> gNoVar
    Just gVar -> choice [gVar, gNoVar]
  
mGenVarWithType :: Context -> Type -> Maybe (Gen Term)
mGenVarWithType ctx ty
  | null vars = Nothing
  | otherwise = Just varWithType
  where
    vars = typeLookup ty ctx
    varWithType = element (Var <$> vars)

genWtBase :: BaseType -> Gen Term
genWtBase UnitT = return Unit
genWtBase NatT  = genConstN
genWtBase BoolT = genConstB

genExactTerm :: TermGenFunc -> Context -> Type -> Gen Term
genExactTerm _genT _ctx (Base baseT)    = genWtBase baseT
genExactTerm  genT  ctx (Arr ty1 ty2)   = genAbsWithTermGen  genT ctx ty1 ty2
genExactTerm  genT  ctx (PairT ty1 ty2) = genPairWithTermGen genT ctx ty1 ty2
genExactTerm _genT _ctx Alias{}         = error "Undefined Gen of Alias"

genMinimalTerm :: Context -> Type -> Gen Term
genMinimalTerm = genExactTerm genMinimalTerm

genTermWithType :: Context -> Type -> Gen Term
genTermWithType ctx ty = 
    recursive choice [base] [exactTerm, genWhateverWithType ctx ty]
  where
    base      =   case mGenVarWithType ctx ty of
                    Just varWithType -> varWithType
                    Nothing          -> genMinimalTerm ctx ty
    exactTerm =   genExactTerm genTermWithType ctx ty

genWhateverWithType :: Context -> Type -> Gen Term
genWhateverWithType ctx ty = 
  case mGenVarWithType ctx ty of
      Just varWithType -> choice (varWithType: withTypes)
      Nothing          -> choice withTypes
  where 
    withTypes = [
          genAppWithType
        , genLetWithType
        , genIfWithType
        , genSeqWithType
        , genAsWithType
        , genProj1WithType
        , genProj2WithType 
        ] <*> [ctx] <*> [ty]

genAppWithType :: Context -> Type -> Gen Term
genAppWithType ctx t12 = do
  t11 <- genType
  subterm2WithType ctx (Arr t11 t12) t11 App

genIfWithType :: Context -> Type -> Gen Term
genIfWithType ctx t =
  subterm3
    (genTermWithType ctx (Base BoolT))
    (genTermWithType ctx t)
    (genTermWithType ctx t)
    IfThenElse

genLetWithType :: Context -> Type -> Gen Term
genLetWithType ctx ty = do
  p   <- genPattern
  ty1 <- genType
  let newCtx = case p of
                  Wildcard     -> ctx
                  Identifier x -> insert (x, ty1) ctx
      t1 = genTermWithType ctx ty1
      t2 = genTermWithType newCtx ty
  subterm2 t1 t2 (Let p)

type TermGenFunc = Context -> Type -> Gen Term
genAbsWithTermGen :: TermGenFunc -> Context -> Type -> Type -> Gen Term
genAbsWithTermGen genT ctx ty1 ty2 = do
  p   <- genPattern
  let newCtx = case p of
                  Wildcard     -> ctx
                  Identifier x -> insert (x, ty1) ctx
  subterm (genT newCtx ty2) (Abs p ty1)

genPairWithTermGen :: TermGenFunc -> Context -> Type -> Type -> Gen Term
genPairWithTermGen genT ctx ty1 ty2 = subterm2 (genT ctx ty1) (genT ctx ty2) Pair

subterm2WithType :: Context -> Type -> Type -> (Term -> Term -> Term) -> Gen Term
subterm2WithType ctx ty1 ty2 = subterm2 (genTermWithType ctx ty1) (genTermWithType ctx ty2)

genSeqWithType :: Context -> Type -> Gen Term
genSeqWithType ctx ty = subterm2WithType ctx (Base UnitT) ty Seq

genAsWithType :: Context -> Type -> Gen Term
genAsWithType ctx ty = subtermWithType ctx ty (`As` ty)

subtermWithType :: Context -> Type -> (Term -> Term) -> Gen Term
subtermWithType ctx ty = subterm (genTermWithType ctx ty)

genProj1WithType :: Context -> Type -> Gen Term
genProj1WithType ctx ty = do
  pty <- PairT ty <$> genType
  subtermWithType ctx pty Proj1

genProj2WithType :: Context -> Type -> Gen Term
genProj2WithType ctx ty = do
  pty <- flip PairT ty <$> genType
  subtermWithType ctx pty Proj2

genWtVar :: Context -> Maybe (Gen (Term, Type))
genWtVar ctx 
  | isEmpty ctx = Nothing
  | otherwise = Just . element $ first Var <$> toList ctx


varNameDB :: [String]
varNameDB = filter (not . (`elem` keywords)) names 
  where
    names = (++) <$> aToZ <*> "" : aToZ
    charToString = (:[])
    aToZ = map charToString ['a'..'z']
    keywords = [
        "if"
      , "then"
      , "else"
      , "let"
      , "in"
      , "true"
      , "false"
      , "succ"
      , "pred"
      , "iszero"
      , "unit"
      , "Bool"
      , "Nat"
      , "Unit"
      , "as"
      , "_"] 


genVarName :: Gen String
genVarName = prune (element varNameDB)
  -- (:) <$> lower <*> listOf letter

genNat :: Gen Int
genNat = prune $ int (Range.constant 0 10000)

genConstN :: Gen Term
genConstN =  ConstN <$> genNat

genConstB :: Gen Term
genConstB = ConstB <$> element [ConstTrue, ConstFalse]

genPattern :: Gen Pattern
genPattern = choice [Identifier <$> genVarName, return Wildcard]
