module PPrint(pshow, pprint, qshow) where
import Types

class PPrint a where
  pshow :: a -> String
  
  pprint :: a -> IO ()
  pprint = putStr . pshow

instance PPrint Term where
  pshow (Abs x ty t) = "λ" ++ x ++ ": " ++ pshow ty ++ ". " ++ pshow t
  pshow (App x y)     = pshowApp1 x ++ " " ++ pshowApp2 y
  pshow (Var x)       = x
  pshow (ConstB ConstTrue) = "true"
  pshow (ConstB ConstFalse) = "false"
  pshow (IfThenElse t1 t2 t3) = "if " ++ pshow t1 ++ " then " ++ pshow t2 ++ " else " ++ pshow t3
  pshow (ConstN n) = show (magnitude n)
  pshow (Succ t) = "succ " ++ pshowApp2 t
  pshow (Pred t) = "pred " ++ pshowApp2 t
  pshow (IsZero t) = "iszero " ++ pshowApp2 t
  pshow Unit = "unit"
  
pshowApp1 :: Term -> String
pshowApp1 t@Abs{}        = parenthesize (pshow t)
pshowApp1 t@IfThenElse{} = parenthesize (pshow t)
pshowApp1 t@Succ{}       = parenthesize (pshow t)
pshowApp1 t@Pred{}       = parenthesize (pshow t)
pshowApp1 t@IsZero{}     = parenthesize (pshow t)
pshowApp1 t              = pshow t

pshowApp2 :: Term -> String
pshowApp2 t@Abs{}        = parenthesize (pshow t)
pshowApp2 t@App{}        = parenthesize (pshow t)
pshowApp2 t@IfThenElse{} = parenthesize (pshow t)
pshowApp2 t@Succ{}       = parenthesize (pshow t)
pshowApp2 t@Pred{}       = parenthesize (pshow t)
pshowApp2 t@IsZero{}     = parenthesize (pshow t)
pshowApp2 t              = pshow t

parenthesize :: String -> String
parenthesize str = "(" ++ str ++ ")"

instance PPrint Type where
  pshow (Arr t1 t2)  = pshowArr1 t1 ++ " -> " ++ pshow t2
  pshow (Base BoolT) = "Bool"
  pshow (Base NatT)  = "Nat"
  pshow (Base UnitT)  = "Unit"

pshowArr1 :: Type -> String
pshowArr1 t@Arr{}  = parenthesize (pshow t)
pshowArr1 t@Base{} = pshow t

qshow :: PPrint a => a -> String
qshow t = "'" ++ pshow t ++ "'"