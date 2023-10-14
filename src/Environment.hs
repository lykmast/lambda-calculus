module Environment(Environment, envLookup, envInsert, emptyEnv) where

import qualified Data.Map as M
import Syntax(Type, Term, Var)

type Environment = M.Map Var (Type, Term)

envLookup :: Environment -> Var -> Maybe (Type, Term)
envLookup = flip M.lookup

envInsert :: Var -> (Type, Term) -> Environment -> Environment
envInsert = M.insert

emptyEnv :: Environment
emptyEnv = M.empty