module PPrint(pshow, pprint, qshow) where
import Syntax

class PPrint a where
  pshow :: a -> String
  
  pprint :: a -> IO ()
  pprint = putStr . pshow

instance PPrint Term where
  pshow (Abs x ty t)  = "λ" ++ pshow x ++ ": " ++ pshow ty ++ ". " ++ pshow t
  pshow (App x y)     = pshowApp1 x ++ " " ++ pshowApp2 y
  pshow (Let p t1 t2) = "let " ++ pshow p ++ " = " ++ pshow t1 ++ " in " ++ pshow t2
  pshow (Seq x y)     = pshow x ++ "; " ++ pshow y
  pshow (As t ty)     = pshowAs1 t ++ " as " ++ pshow ty 
  pshow (Var x)       = x
  pshow (ConstB ConstTrue) = "true"
  pshow (ConstB ConstFalse) = "false"
  pshow (IfThenElse t1 t2 t3) = "if " ++ pshow t1 ++ " then " ++ pshow t2 ++ " else " ++ pshow t3
  pshow (ConstN n) = show (magnitude n)
  pshow (Succ t) = "succ " ++ pshowApp2 t
  pshow (Pred t) = "pred " ++ pshowApp2 t
  pshow (IsZero t) = "iszero " ++ pshowApp2 t
  pshow Unit = "unit"

pshowAs1 :: Term -> String
pshowAs1 t@Abs{}        = parenthesize (pshow t)
pshowAs1 t@IfThenElse{} = parenthesize (pshow t)
pshowAs1 t              = pshow t

pshowApp1 :: Term -> String
pshowApp1 t@Abs{}        = parenthesize (pshow t)
pshowApp1 t@IfThenElse{} = parenthesize (pshow t)
pshowApp1 t@Let{}        = parenthesize (pshow t)
pshowApp1 t              = pshow t

pshowApp2 :: Term -> String
pshowApp2 t@Abs{}        = parenthesize (pshow t)
pshowApp2 t@App{}        = parenthesize (pshow t)
pshowApp2 t@As{}         = parenthesize (pshow t)
pshowApp2 t@IfThenElse{} = parenthesize (pshow t)
pshowApp2 t@Succ{}       = parenthesize (pshow t)
pshowApp2 t@Pred{}       = parenthesize (pshow t)
pshowApp2 t@IsZero{}     = parenthesize (pshow t)
pshowApp2 t@Let{}        = parenthesize (pshow t)
pshowApp2 t              = pshow t

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