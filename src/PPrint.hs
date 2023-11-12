module PPrint(pshow, pprint, qshow) where
import Syntax

class PPrint a where
  pshow :: a -> String

  pprint :: a -> IO ()
  pprint = putStr . pshow

instance PPrint Term where
  pshow = pshowSeq

parenthesize :: String -> String
parenthesize str = "(" ++ str ++ ")"

instance PPrint Type where
  pshow (Arr t1 t2)   = pshowArr1 t1 ++ " -> " ++ pshow t2
  pshow (Base BoolT)  = "Bool"
  pshow (Base NatT)   = "Nat"
  pshow (Base UnitT)  = "Unit"
  pshow (Alias x)     = x
  pshow (PairT t1 t2) = pshowPairSubT t1 ++ " " ++ xMult ++ " " ++ pshowPairSubT t2
    where xMult = "\215"

pshowArr1 :: Type -> String
pshowArr1 t@Arr{}   = parenthesize (pshow t)
pshowArr1 t@Base{}  = pshow t
pshowArr1 t@Alias{} = pshow t
pshowArr1 t@PairT{} = pshow t

pshowPairSubT :: Type -> String
pshowPairSubT t@Arr{}   = parenthesize (pshow t)
pshowPairSubT t@PairT{} = parenthesize (pshow t)
pshowPairSubT t@Base{}  = pshow t
pshowPairSubT t@Alias{} = pshow t

qshow :: PPrint a => a -> String
qshow t = "'" ++ pshow t ++ "'"

instance PPrint Pattern where
  pshow (Identifier x) = x
  pshow Wildcard       = "_"


pshowSeq :: Term -> String
pshowSeq (Seq t1 t2) = pshowSeq t1 ++ "; " ++ pshowNonSeq t2
pshowSeq t           = pshowNonSeq t

pshowNonSeq :: Term -> String
pshowNonSeq (As t ty) = pshowAs t ty
pshowNonSeq t         = pshowAbsLike t

pshowAbsLike :: Term -> String
pshowAbsLike (Abs p ty t)           = pshowAbs p ty t
pshowAbsLike (Let p t1 t2)          = pshowLet p t1 t2
pshowAbsLike (IfThenElse t1 t2 t3)  = pshowIfThenElse t1 t2 t3
pshowAbsLike t                      = pshowAppLike t

pshowAppLike :: Term -> String
pshowAppLike (Succ t)    = "succ "   ++ pshowAppSubterm t
pshowAppLike (Pred t)    = "pred "   ++ pshowAppSubterm t
pshowAppLike (IsZero t)  = "iszero " ++ pshowAppSubterm t
pshowAppLike (App t1 t2) = pshowAppLike t1 ++ " " ++ pshowAppSubterm t2
pshowAppLike t           = pshowAppSubterm t

pshowAppSubterm :: Term -> String
pshowAppSubterm (Proj1 t) = pshowAppSubterm t ++ ".1"
pshowAppSubterm (Proj2 t) = pshowAppSubterm t ++ ".2"
pshowAppSubterm t         = pshowAtomTerm t

pshowAtomTerm :: Term -> String
pshowAtomTerm (ConstN n)          = show n
pshowAtomTerm (ConstB ConstTrue)  = "true"
pshowAtomTerm (ConstB ConstFalse) = "false"
pshowAtomTerm Unit                = "unit"
pshowAtomTerm (Var x)             = x
pshowAtomTerm (Pair t1 t2)        = "{" ++ pshowSeq t1 ++ ", " ++ pshowSeq t2 ++ "}"
pshowAtomTerm t                   = parenthesize (pshowSeq t)


pshowIfThenElse :: Term -> Term -> Term -> String
pshowIfThenElse t1 t2 t3= "if " ++ pshowNonSeq t1 ++ " then " ++ pshowNonSeq t2 ++ " else " ++ pshowNonSeq t3

pshowAbs :: Pattern -> Type -> Term -> String
pshowAbs p ty t = "λ" ++ pshow p ++ ": " ++ pshow ty ++ ". " ++ pshowNonSeq t

pshowLet :: Pattern -> Term -> Term -> String
pshowLet p t1 t2 = "let " ++ pshow p ++ " = " ++ pshowNonSeq t1 ++ " in " ++ pshowNonSeq t2

pshowAs :: Term -> Type -> String
pshowAs t ty = pshowAppLike t ++ " as " ++ pshow ty