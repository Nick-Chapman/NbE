
module Meh.Eval_Standard(evaluate) where

import Control.Monad(ap,liftM)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

evaluate :: Exp -> Value
evaluate = runM . eval

data Value
  = Number Int
  | Function Fun
  | Vadd0
  | Vadd1 Value
  deriving (Show)

newtype Fun = Fun (Value -> M Value)
instance Show Fun where show _ = "<Fun>"

eval :: Exp -> M Value
eval = \case
  Num n -> do
    return $ Number n
  AddOp->
    return Vadd0
  SaturatedAdd e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    return $ add v1 v2
  Var x -> do
    Lookup x
  Lam x body -> do
    env <- Save
    return $ Function $ Fun $ \arg -> do
      Restore env $ ModEnv (Map.insert x arg) $ eval body
  App e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    apply v1 v2
  Let x rhs body -> do
    eval (App (Lam x body) rhs)

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"

apply :: Value -> Value -> M Value
apply = \case
  Function (Fun f) -> \arg -> f arg
  Vadd0 -> \arg -> return $ Vadd1 arg
  Vadd1 v1 -> \v2 -> return $ add v1 v2
  f -> error $ "can't apply a non function: " <> show f

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
