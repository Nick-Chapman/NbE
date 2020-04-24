
module Main(main) where

import Ast
import Eval_Instrumented(evaluate)
import qualified Cek(evaluate)
import qualified Cek2(evaluate)
import qualified Cek3(evaluate)
--import Norm_Final(normalize)

main :: IO ()
main = do
  demo "original" prog
  --demo "optimized" (normalize prog)

demo :: String -> Exp -> IO ()
demo tag prog = do
  print tag
  print prog
  print "Eval"; print (evaluate (App prog (Num 9)))
  print "CEK"; print (Cek.evaluate (App prog (Num 9)))
  print "CEK2"; print (Cek2.evaluate (App prog (Num 9)))
  print "CEK3"; print (Cek3.evaluate (App prog (Num 9)))

prog :: Exp
prog = Lam "arg" (
  Let "dub" (Lam "x" (Add (Var "x") (Var "x"))) (
  Let "twice" (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x"))))) (
  Let "increase" (Lam "q" (Lam "x" (Add (App (Var "dub") (Var "x")) (Var "q")))) (
  App (App (Var "twice") (App (Var "increase") (Add (Var "arg") (Num 1)))) (Num 3)))))
