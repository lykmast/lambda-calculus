module Main (main) where

import PPrint(pprint)
import Eval (eval)
import Parser(theParser)
import GHC.IO.Handle (hFlush, isEOF)
import System.IO (stdout)
import Typecheck(typecheckTop)
import Environment(Environment, envInsert, emptyEnv)
import Data.Bifunctor(first)
import Syntax

main :: IO ()
main = repl emptyEnv

repl :: Environment -> IO ()
repl env = do
  putStr "λ> "
  hFlush stdout
  unlessEOF $ do
    inp <- getLine
    case theParser inp of
      Left e -> do
        putStrLn "Parse error:"
        print e
        repl env
      Right top ->
        case handleTop top of
          Left e -> do
            putStrLn e
            repl env
          Right (ty, res, newEnv) -> do
            pprint res
            putStr "\n:: "
            pprint ty
            putStrLn ""
            repl newEnv
  where
    handleTop :: TopLevel -> Either String (Type, Term, Environment)
    handleTop (Left t) = do
      ty  <- first ("Type error:\n" ++ ) $ typecheckTop env t 
      res <- first ("Eval error:\n" ++ ) $ eval env t
      return (ty, res, env)
    handleTop (Right (x, t)) = do
      ty  <- first ("Type error:\n" ++ ) $ typecheckTop env t 
      res <- first ("Eval error:\n" ++ ) $ eval env t
      return (ty, res, envInsert x (ty, res) env)

    unlessEOF :: IO () -> IO ()
    unlessEOF act = do
      done <- isEOF
      if done then putStrLn "\nOk bye."
      else act