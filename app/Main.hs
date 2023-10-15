module Main (main) where

import PPrint(pprint, pshow)
import Parser(theParser)
import GHC.IO.Handle (hFlush, isEOF)
import System.IO (stdout)
import TopLevel(topEval, Result(..))
import Environment(Environment, emptyEnv)

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
        case topEval env top of
          Left e -> do
            putStrLn e
            repl env
          Right (res, newEnv) -> do
            handleResult res
            repl newEnv
  where
    handleResult :: Result -> IO ()
    handleResult (ResTerm t ty) = do
      pprint t
      putStr "\n:: "
      pprint ty
      putStrLn ""
    handleResult (ResType x ty) = putStrLn $ x ++ " = " ++ pshow ty

    unlessEOF :: IO () -> IO ()
    unlessEOF act = do
      done <- isEOF
      if done then putStrLn "\nOk bye."
      else act