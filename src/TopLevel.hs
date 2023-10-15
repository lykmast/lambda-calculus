module TopLevel(topEval, Result(..)) where

import Syntax
import Eval(eval)
import Typecheck(typecheck)
import Environment(Environment, termInsert, typeAliasInsert)
import Context(fromEnvironment)
import Data.Bifunctor(first)

typecheckTop :: Environment -> Term  -> Either String Type
typecheckTop env = typecheck (fromEnvironment env)

data Result = ResTerm Term Type | ResType Var Type

topEval :: Environment -> TopLevel -> Either String (Result, Environment)
topEval env (TopTerm t) = do
  (ty, t') <- typecheckEval env t
  return (ResTerm t' ty, env)
topEval env (TopBind b) = bindEval env b

bindEval :: Environment -> Binding -> Either String (Result, Environment)
bindEval env (TermBind x t) = do
  (ty, t') <- typecheckEval env t
  let newEnv = termInsert x (ty, t') env
  return (ResTerm t' ty, newEnv) 
bindEval env (TypeBind x ty) = Right (ResType x ty, typeAliasInsert x ty env) 

typecheckEval :: Environment -> Term -> Either String (Type, Term)
typecheckEval env t = do
  ty <- first ("Type error:\n" ++ ) $ typecheckTop env t
  t' <- first ("Eval error:\n" ++ ) $ eval env t
  return (ty, t')
  