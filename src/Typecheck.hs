module Typecheck(typecheck) where

import Syntax
import PPrint (qshow)
import Context(Context, varTypeLookup, typeAliasLookup, insertVarType)

typecheck :: Context -> Term -> Either String Type
typecheck c (Var x) =
    case varTypeLookup c x of
      Just t -> Right t
      Nothing -> Left $ "No " ++ qshow (Var x)  ++ " in type context."
typecheck c (Abs p ty t) =
  let c' =  case p of
              Identifier x -> insertVarType x ty c
              Wildcard     -> c
  in Arr ty <$> typecheck c' t
typecheck c (App t1 t2)  = do
    ty1 <- typecheck c t1
    ty2 <- typecheck c t2
    case toArrType c ty1 of
      Just (ty11, ty12) | typeEq c ty11 ty2 -> return ty12
      _ -> Left $ "Term " ++ qshow t1  ++ " with type " ++ qshow ty1 ++ 
                  " cannot be applied to " ++ qshow t2  ++ " with type " ++ qshow ty2  ++ "."
typecheck c (Let p t1 t2) = do
  ty <- typecheck c t1
  let c' =  case p of
              Identifier x -> insertVarType x ty c
              Wildcard     -> c
  typecheck c' t2
typecheck c t@(Seq t1 t2) = do
  ty1 <- typecheck c t1
  ty2 <- typecheck c t2
  if typeEq c ty1 (Base UnitT)
    then Right ty2
    else Left $ "Term " ++ qshow t1 ++ " should be of type " ++
                          qshow (Base UnitT) ++ " in " ++ qshow t ++ "."
typecheck c (As t1 ty) = do
  ty1 <- typecheck c t1
  if typeEq c ty1 ty
    then return ty
    else Left $ "Term " ++ qshow t1 ++ " cannot be ascribed with type " ++ qshow ty ++ "."
typecheck _c (ConstB _) = Right (Base BoolT)
typecheck _c (ConstN _) = Right (Base NatT)
typecheck c t@(IfThenElse t1 t2 t3) = do
  ty1 <- typecheck c t1
  ty2 <- typecheck c t2
  ty3 <- typecheck c t3
  if typeEq c ty1 (Base BoolT)
    then 
      if typeEq c ty2 ty3 
        then Right ty2
        else Left $ "Terms " ++ qshow t1  ++ " of type " ++ qshow ty1 ++ 
                    " and " ++ qshow t2  ++ " of type " ++ qshow ty2 ++ 
                    " should have the same type in " ++ qshow t  ++ "."
    else Left $ "Term " ++ qshow t1  ++ " should have type 'bool' in " ++ qshow t  ++ "."
typecheck c t@(Succ t1) = do
    ty <- typecheck c t1
    if typeEq c ty (Base NatT)
      then Right (Base NatT)
      else Left $ "Term " ++ qshow t1  ++ " should have type " ++ qshow (Base NatT)  ++ " in " ++ qshow t  ++ "."
typecheck c t@(Pred t1) = do
    ty <- typecheck c t1
    if typeEq c ty (Base NatT)
      then Right (Base NatT)
      else Left $ "Term " ++ qshow t1  ++ " should have type " ++ qshow (Base NatT)  ++ " in " ++ qshow t  ++ "."
typecheck c t@(IsZero t1) = do
    ty <- typecheck c t1
    if typeEq c ty (Base NatT)
      then Right (Base BoolT)
      else Left $ "Term " ++ qshow t1  ++ " should have type " ++ qshow (Base NatT)  ++ " in " ++ qshow t  ++ "."
typecheck _c Unit = Right (Base UnitT) 


typeEq :: Context -> Type -> Type -> Bool
typeEq c  (Alias x)     ty2 = 
    case typeAliasLookup c x of
      Nothing  -> False
      Just ty1 -> typeEq c ty1 ty2
typeEq c  ty1           (Alias x) =
    case typeAliasLookup c x of
      Nothing  -> False
      Just ty2 -> typeEq c ty1 ty2
typeEq c  (Arr t11 t12) (Arr t21 t22) = typeEq c t11 t21 && typeEq c t12 t22
typeEq _c (Base BoolT)  (Base BoolT)  = True
typeEq _c (Base NatT)   (Base NatT)   = True
typeEq _c (Base UnitT)  (Base UnitT)  = True

typeEq _c (Base BoolT)   _            = False
typeEq _c (Base NatT)    _            = False
typeEq _c (Base UnitT)   _            = False
typeEq _c Arr{}          Base{}       = False

toArrType :: Context -> Type -> Maybe (Type, Type)
toArrType _c (Arr t1 t2) = Just (t1, t2)
toArrType c  (Alias x)   = toArrType c =<< typeAliasLookup c x 
toArrType _c _ty         = Nothing