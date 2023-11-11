module Context(
    Context
  , varTypeLookup
  , typeAliasLookup
  , insertVarType
  , fromEnvironment
  , empty
  ) where

import Syntax ( Type, Var )
import Environment (getTermTypes, getTypeAliases, Environment)
import qualified Data.Map as M

data Context = Context {varTypes :: M.Map Var Type, typeAliases :: M.Map Var Type}

varTypeLookup :: Context -> Var -> Maybe Type
varTypeLookup = flip M.lookup . varTypes

typeAliasLookup :: Context -> Var -> Maybe Type
typeAliasLookup = flip M.lookup . typeAliases

insertVarType :: Var -> Type -> Context -> Context
insertVarType x ty c = c{varTypes = M.insert x ty (varTypes c)}

fromEnvironment :: Environment -> Context
fromEnvironment env = Context {varTypes = getTermTypes env, typeAliases = getTypeAliases env}

empty :: Context
empty = Context M.empty M.empty