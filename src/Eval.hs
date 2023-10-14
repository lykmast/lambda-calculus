module Eval(eval) where
import Syntax
import PPrint (qshow)
import Environment(Environment, envLookup)

eval :: Environment -> Term -> Either String Term
eval env = go
  where
    go t@(Var x) =
      case envLookup env x of
        Just (_ty, t') -> go t'
        Nothing -> Left $ "Unbound variable " ++ qshow t ++ "'."
    go (App t1 t2) = do
      v1 <- go t1
      v2 <- go t2
      case (v1, v2) of
        (Abs (Identifier x) _ty t,   yterm) -> go (replace x yterm t)
        (Abs Wildcard       _ty t,  _yterm) -> go t
        (t1', t2') -> Left $ evalErrMsg (App t1' t2')
    go (Seq t1 t2) = do
      v1 <- go t1
      case v1 of 
        Unit -> go t2
        _    -> Left $ evalErrMsg (Seq v1 t2)
    go (IfThenElse t1 t2 t3) = do
      v1 <- go t1
      case v1 of
        ConstB ConstTrue -> go t2
        ConstB ConstFalse -> go t3
        _ -> Left $ evalErrMsg (IfThenElse v1 t2 t3)
    go (Succ t) = do
      v <- go t
      case v of
        ConstN n -> Right $ ConstN (n + 1)
        _        -> Left  $ evalErrMsg (Succ v)
    go (Pred t) = do
      v <- go t
      case v of
        ConstN n | n > 0 -> Right $ ConstN (n - 1)
        _ -> Left $ evalErrMsg (Pred v)
    go (IsZero t) = do
      v <- go t
      case v of
        ConstN 0 -> Right $ ConstB ConstTrue
        ConstN _ -> Right $ ConstB ConstFalse
        _        -> Left $ evalErrMsg (IsZero v)
    go t@ConstB{} = Right t
    go t@ConstN{} = Right t
    go t@Abs{}    = Right t
    go t@Unit     = Right t

evalErrMsg :: Term -> String
evalErrMsg t = "Can't evaluate " ++ qshow t ++ "."

replace :: Var -> Term -> Term -> Term
replace x yterm (Var x')
  | x' == x   = yterm
  | otherwise =  Var x'
replace x yterm (Abs (Identifier x') ty t)
  | x' == x   = Abs (Identifier x') ty t
  | otherwise = Abs (Identifier x') ty (replace x yterm t)
replace x yterm (Abs Wildcard ty t) = Abs Wildcard ty (replace x yterm t)
replace x yterm (App t1 t2) =
    App (replace x yterm t1) (replace x yterm t2)
replace x yterm (Seq t1 t2) =
    Seq (replace x yterm t1) (replace x yterm t2)
replace x yterm (IfThenElse t1 t2 t3) =
    IfThenElse (replace x yterm t1) (replace x yterm t2) (replace x yterm t3)
replace x yterm (Succ t) = Succ (replace x yterm t)
replace x yterm (Pred t) = Pred (replace x yterm t)
replace x yterm (IsZero t) = IsZero (replace x yterm t)
replace _x _yterm t@ConstB{} = t
replace _x _yterm t@ConstN{} = t
replace _x _yterm t@Unit     = t