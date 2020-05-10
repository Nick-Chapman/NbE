
module Meh.Eval_Environment(evaluate) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

evaluate :: Exp -> Value
evaluate = eval Map.empty

data Value
  = Number Int
  | Function Fun
  | Vadd0
  | Vadd1 Value
  deriving (Show)

newtype Fun = Fun (Value -> Value)
instance Show Fun where show _ = "<Fun>"

type Env = Map Var Value

eval :: Env -> Exp -> Value
eval env = \case
  Num n -> do
    Number n
  AddOp ->
    Vadd0
  SaturatedAdd e1 e2 -> do
    let v1 = eval env e1
    let v2 = eval env e2
    add v1 v2
  Var x ->
    look env x
  Lam x body -> do
    Function $ Fun $ \arg -> do
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
add _ _ = error "can't add non-numbers"

apply :: Value -> Value -> Value
apply = \case
  Function (Fun f) -> \arg -> f arg
  Vadd0 -> Vadd1
  Vadd1 v1 -> add v1
  f -> error $ "can't apply a non function: " <> show f

look :: Env -> Var -> Value
look env x = maybe (error $ "lookup:"<>x) id (Map.lookup x env)
