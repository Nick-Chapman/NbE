
module Eval_Environment(evaluate) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

evaluate :: Exp -> Int
evaluate = unNumber . eval Map.empty

data Value
  = Number { unNumber :: Int }
  | Function (Value -> Value)

type Env = Map Var Value

eval :: Env -> Exp -> Value
eval env = \case
  Num n -> do
    Number n
  Add e1 e2 -> do
    let v1 = eval env e1
    let v2 = eval env e2
    add v1 v2
  Var x ->
    look env x
  Lam x body -> do
    Function $ \arg -> do
      eval (Map.insert x arg env) body
  App e1 e2 -> do
    let v1 = eval env e1
    let v2 = eval env e2
    apply v1 v2
  Let x rhs body -> do
    let v = eval env rhs
    eval (Map.insert x v env) body

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "add"

apply :: Value -> Value -> Value
apply (Function f) arg = f arg
apply _ _ = error "apply"

look :: Env -> Var -> Value
look env x = maybe (error $ "lookup:"<>x) id (Map.lookup x env)
