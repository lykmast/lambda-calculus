module Typecheck(typecheck, typecheckTop) where


import qualified Data.Map as M
import Util(Var, )
import Environment(Environment)
import Types
import PPrint (pshow)
-- import Data.Maybe (isNothing)
type Context = M.Map Var Type


contextFromEnvironment :: Environment -> Context
contextFromEnvironment = fmap fst

-- emptyContext :: Context
-- emptyContext = M.empty

typecheckTop :: Environment -> Term  -> Either String Type
typecheckTop env = typecheck (contextFromEnvironment env)

typecheck :: Context -> Term -> Either String Type
typecheck c (Var x) =
    case M.lookup x c of
      Just t -> Right t
      Nothing -> Left $ "No '" ++ pshow (Var x) ++ "' in type context."
typecheck c (Abs x ty t) = Arr ty <$> typecheck (M.insert x ty c) t
typecheck c (App t1 t2)  = do
    ty1 <- typecheck c t1
    ty2 <- typecheck c t2
    case ty1 of
      Arr ty11 ty12 | typeEq c ty11 ty2 -> return ty12
      _ -> Left $ "Term '" ++ pshow t1 ++ "' with type '" ++ pshow ty1 ++ 
                  "' cannot be applied to '" ++ pshow t2 ++ "' with type '" ++ pshow ty2 ++ "'."  
typecheck _c (ConstB _) = Right (Base BoolT)
typecheck _c (ConstN _) = Right (Base NatT)
typecheck c t@(IfThenElse t1 t2 t3) = do
  ty1 <- typecheck c t1
  ty2 <- typecheck c t2
  ty3 <- typecheck c t3
  case ty1 of
    Base BoolT -> 
      if typeEq c ty2 ty3 
        then Right ty2
        else Left $ "Terms '" ++ pshow t1 ++ "' of type '" ++ pshow ty1 ++ 
                    "' and '" ++ pshow t2 ++ "' of type '" ++ pshow ty2 ++ 
                    "' should have the same type in '" ++ pshow t ++ "'."
    _ -> Left $ "Term '" ++ pshow t1 ++ "' should have type 'bool' in '" ++ pshow t ++ "'."
typecheck c t@(Succ t1) = do
    ty <- typecheck c t1
    case ty of
      Base NatT -> Right (Base NatT)
      _    -> Left $ "Term '" ++ pshow t1 ++ "' should have type '" ++ pshow (Base NatT) ++ "' in '" ++ pshow t ++ "'."
typecheck c t@(Pred t1) = do
    ty <- typecheck c t1
    case ty of
      Base NatT -> Right (Base NatT)
      _    -> Left $ "Term '" ++ pshow t1 ++ "' should have type '" ++ pshow (Base NatT) ++ "' in '" ++ pshow t ++ "'."
typecheck c t@(IsZero t1) = do
    ty <- typecheck c t1
    case ty of
      Base NatT -> Right (Base BoolT)
      _    -> Left $ "Term '" ++ pshow t1 ++ "' should have type '" ++ pshow (Base NatT) ++ "' in '" ++ pshow t ++ "'."



typeEq :: Context -> Type -> Type -> Bool
typeEq _c (TVar _) _ = True -- x == y || any isNothing (M.lookup c <$> [x,y])
typeEq _c _ (TVar _) = True
typeEq c (Arr t11 t12) (Arr t21 t22) = typeEq c t11 t21 && typeEq c t12 t22
typeEq _c (Base BoolT) (Base BoolT) = True
typeEq _c (Base NatT) (Base NatT) = True
typeEq _c _ _ = False
