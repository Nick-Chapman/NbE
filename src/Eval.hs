
module Eval(evaluate) where

import Control.Monad(ap,liftM)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

evaluate :: Exp -> Int
evaluate = unNumber . runM . eval

data Value
  = Number { unNumber :: Int }
  | Function (Value -> M Value)

eval :: Exp -> M Value
eval = \case
  Num n -> do
    return $ Number n
  Add e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    return $ add v1 v2
  Var x -> do
    Lookup x
  Lam x body -> do
    env <- Save
    return $ Function $ \arg -> do
      Restore env $ ModEnv (Map.insert x arg) $ eval body
  App e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    apply v1 v2
  Let x rhs body -> do
    eval (App (Lam x body) rhs)

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "add"

apply :: Value -> Value -> M Value
apply (Function f) arg = f arg
apply _ _ = error "apply"

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Lookup :: Var -> M Value
  ModEnv :: (Env -> Env) -> M a -> M a
  Save :: M Env
  Restore :: Env -> M a -> M a

runM :: M Value -> Value
runM m = loop Map.empty m where
  loop :: Env -> M a -> a
  loop env = \case
    Ret x -> x
    Bind m f -> let a = loop env m in loop env (f a)
    Lookup x -> maybe (error $ "lookup:"<>x) id (Map.lookup x env)
    ModEnv f m -> loop (f env) m
    Save -> env
    Restore env m -> loop env m

type Env = Map Var Value
