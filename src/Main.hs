
module Main(main) where

import Ast
--import Eval_ExplicitSubst(evaluate)
--import Eval_Environment(evaluate)
--import Eval(evaluate)
--import Eval_Instrumented(evaluate)
import qualified Cek(evaluate)
--import Cek4(evaluate)

import qualified Cek5(compile,execute)
import Norm_Final(normalize)

import qualified BC1(compile,execute)

main :: IO ()
main = do
  demo "original" prog
  demo "optimized" (normalize prog)

demo :: String -> Exp -> IO ()
demo tag prog = do
  print tag
  print prog
  let exp = App prog (Num 9)
  print "Eval"; print (Cek.evaluate exp)

  print "CEK5"
  let code = Cek5.compile exp
  print code; print (Cek5.execute code)

  print "ByteCode"
  let code = BC1.compile exp
  print code; print (BC1.execute code)


prog :: Exp
prog = Lam "arg" (
  mkLet "dub" (Lam "x" (add (Var "x") (Var "x"))) (
  mkLet "thrice" (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))) (
  mkLet "increase" (Lam "q" (Lam "x" (mkLet "dubX" (App (Var "dub") (Var "x")) (add (Var "dubX") (Var "q"))))) (
  App (App (Var "thrice") (App (Var "increase") (add (Var "arg") (Num 1)))) (Num 3)))))

  where
    --mkLet x rhs body = App (Lam x body) rhs
    mkLet = Let

    --add e1 e2 = App (App AddOp e1) e2
    add = SaturatedAdd
