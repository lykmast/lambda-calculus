module Eval(eval, evalWithEnv) where
import Types
import PPrint (qshow)
import Util (Var)
import Environment(Environment, envLookup)

eval :: Term -> Term
eval t@Abs{} = t
eval (App t1 t2) =
  case (eval t1, eval t2) of
    (Abs x _ty t, yterm) -> eval (replace x yterm t)
    (t1', t2')           -> App t1' t2'
eval (IfThenElse t1 t2 t3) =
  case eval t1 of
    ConstB ConstTrue -> eval t2
    ConstB ConstFalse -> eval t3
    t1' -> IfThenElse t1' t2 t3
eval (Succ t) = 
  case eval t of
    ConstN n -> ConstN (n + 1)
    v        -> Succ v 
eval (Pred t) =
    case eval t of
      ConstN n | n > 0 -> ConstN (n - 1)
      v -> Pred v
eval (IsZero t) =
    case eval t of
      ConstN 0 -> ConstB ConstTrue
      ConstN _ -> ConstB ConstFalse
      v        -> IsZero v
eval t@Var{} = t
eval t@ConstB{} = t
eval t@ConstN{} = t


evalWithEnv :: Environment -> Term -> Either String Term
evalWithEnv env = go
  where 
    go t@(Var x) = 
      case envLookup env x of
        Just (_ty, t') -> go t'
        Nothing -> Left $ "Unbound variable " ++ qshow t ++ "'."
    go (App t1 t2) = do
      v1 <- go t1
      v2 <- go t2
      case (v1, v2) of
        (Abs x _ty t, yterm) -> go (replace x yterm t)
        (t1', t2')           -> Left $ evalErrMsg (App t1' t2')
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

evalErrMsg :: Term -> String
evalErrMsg t = "Can't evaluate " ++ qshow t ++ "." 

replace :: Var -> Term -> Term -> Term
replace x yterm (Var x')
  | x' == x   = yterm
  | otherwise =  Var x'
replace x yterm (Abs x' ty t)
  | x' == x   = Abs x' ty t
  | otherwise = Abs x' ty (replace x yterm t)
replace x yterm (App t1 t2) =
    App (replace x yterm t1) (replace x yterm t2)
replace x yterm (IfThenElse t1 t2 t3) =
    IfThenElse (replace x yterm t1) (replace x yterm t2) (replace x yterm t3)
replace x yterm (Succ t) = Succ (replace x yterm t)
replace x yterm (Pred t) = Pred (replace x yterm t)
replace x yterm (IsZero t) = IsZero (replace x yterm t)
replace _x _yterm t@ConstB{} = t
replace _x _yterm t@ConstN{} = t