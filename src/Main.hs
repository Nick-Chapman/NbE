module Main(main) where

import Ast
--import Eval(evaluate)
import Eval_Instrumented(evaluate)
import Norm_Final(normalize)

main :: IO ()
main = do
  print (_original 9)
  print (_goal 9)
  demo "original" prog
  let progN = normalize prog
  demo "normalized" progN


demo :: String -> Exp -> IO ()
demo tag prog = do
  print tag
  print prog
  print (evaluate (App prog (Num 9)))

prog :: Exp
prog = Lam "arg" (
  Let "dub" (Lam "x" (Add (Var "x") (Var "x"))) (
  Let "twice" (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x"))))) (
  Let "increase" (Lam "q" (Lam "x" (Add (App (Var "dub") (Var "x")) (Var "q")))) (
  App (App (Var "twice") (App (Var "increase") (Add (Var "arg") (Num 1)))) (Num 3)))))



-- Haskell versions for reference

_original :: Int -> Int
_original = \arg -> do
  let dub x = x + x
  let twice f x = f (f x)
  let increase q x = dub x + q
  twice (increase (arg+1)) 3

_goal :: Int -> Int
_goal = \arg -> do
  let q = arg + 1
  let x0 = 3
  let x1 = (x0 + x0) + q
  (x1 + x1) + q
