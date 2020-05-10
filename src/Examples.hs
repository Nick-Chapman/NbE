
module Examples(examples) where
-- Various examples to play with when working on evaluators

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

examples :: Map String Exp
examples = Map.fromList
  [ ("num42", Num 42)
  , ("sum", add (Num 42) (Num 13))
  , ("sum4", add (add (Num 1) (Num 2)) (add (Num 3) (Num 4)))
  , ("id", identity)
  , ("id9", App identity (Num 9))
  , ("dub", dub)
  , ("dub9", App dub (Num 9))
  , ("thrice", App (App thrice dub) (Num 9))
  , ("thrice-thrice", thrice_thrice_example)

  , ("dive", App dive (Num 9))
  , ("diveX", App diveX (Num 9))
  ]

identity :: Exp
identity = Lam "x" (Var "x")

dub :: Exp
dub = Lam "x" (add (Var "x") (Var "x"))

decrement :: Exp
decrement = Lam "x" (add (Var "x") (Num (-1)))

twice :: Exp
twice = Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x"))))

thrice :: Exp
thrice = Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))

thrice_thrice_example :: Exp
thrice_thrice_example =
  Let "thrice" thrice (
  Let "dec" decrement (
  App (App (App (Var "thrice") (Var "thrice")) (Var "dec")) (Num 0)))

-- original example presented at deep dive
dive :: Exp
dive = Lam "arg" (
  mkLet "dub" dub (
  mkLet "twice" twice (
  mkLet "increase" (Lam "q" (Lam "x" (add (App (Var "dub") (Var "x")) (Var "q")))) (
  App (App (Var "twice") (App (Var "increase") (add (Var "arg") (Num 1)))) (Num 3)))))

diveX :: Exp
diveX = Lam "arg" (
  mkLet "dub" dub (
  mkLet "thrice" thrice (
  mkLet "increase" (Lam "q" (Lam "x" (mkLet "dubX" (App (Var "dub") (Var "x")) (add (Var "dubX") (Var "q"))))) (
  App (App (Var "thrice") (App (Var "increase") (add (Var "arg") (Num 1)))) (Num 3)))))


add :: Exp -> Exp -> Exp
add = SaturatedAdd
--add e1 e2 = App (App AddOp e1) e2

mkLet :: Var -> Exp -> Exp -> Exp
--mkLet x rhs body = App (Lam x body) rhs
mkLet = Let

