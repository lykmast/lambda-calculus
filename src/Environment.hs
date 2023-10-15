module Environment(
    Environment
  , termLookup
  , typeLookup
  , termInsert
  , emptyEnv
  , typeAliasInsert
  , typeAliasLookup
  , getTypeAliases
  , getTermTypes) where

import qualified Data.Map as M
import Syntax(Type, Term, Var)

type TermEnv = M.Map Var (Type, Term)

type TypeEnv = M.Map Var Type
data Environment = Env {termEnv :: TermEnv, typeEnv :: TypeEnv}

typeTermLookup :: Environment -> Var -> Maybe (Type, Term)
typeTermLookup = flip M.lookup . termEnv

termLookup :: Environment -> Var -> Maybe Term
termLookup env x = snd <$> typeTermLookup env x

termInsert :: Var -> (Type, Term) -> Environment -> Environment
termInsert x pair env = env{termEnv = M.insert x pair (termEnv env)}

typeLookup :: Environment -> Var -> Maybe Type
typeLookup env x = fst <$> typeTermLookup env x

typeAliasLookup :: Environment -> Var -> Maybe Type
typeAliasLookup = flip M.lookup . typeEnv

typeAliasInsert :: Var -> Type -> Environment -> Environment
typeAliasInsert x ty env = env{typeEnv = M.insert x ty (typeEnv env)}

emptyEnv :: Environment
emptyEnv = Env M.empty M.empty

getTermTypes :: Environment -> TypeEnv
getTermTypes =  fmap fst . termEnv

getTypeAliases :: Environment -> TypeEnv
getTypeAliases =  typeEnv