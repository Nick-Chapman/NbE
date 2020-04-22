module Main(main) where

import Ast
import Eval(evaluate)
--import Norm(normalize)

main :: IO ()
main = do
  demo "original" prog
  --demo "normalized" (normalize prog)
  return ()

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
