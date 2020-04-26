
module Eval_Instrumented(evaluate,Counts(..)) where

import Control.Monad(ap,liftM)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

evaluate :: Exp -> (Value,Counts)
evaluate exp = runM (eval exp)

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
    CountAdd
    return $ add v1 v2
  Var x ->
    Lookup x
  Lam x exp -> do
    env <- Save
    return $ Function $ Fun $ \arg -> do
      Restore env $ ModEnv (Map.insert x arg) $ eval exp
  App e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    CountApp
    apply v1 v2
  Let x rhs body -> do
    v1 <- eval rhs
    ModEnv (Map.insert x v1) $ eval body

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"

apply :: Value -> Value -> M Value
apply = \case
  Function (Fun f) -> \arg -> f arg
  Vadd0 -> \arg -> return $ Vadd1 arg
  Vadd1 v1 -> \v2 -> do CountAdd; return $ add v1 v2
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
  CountApp :: M ()
  CountAdd :: M ()

runM :: M Value -> (Value,Counts)
runM m = loop Map.empty c0 m where
  c0 = Counts 0 0
  loop :: Env -> Counts -> M a -> (a,Counts)
  loop env counts = \case
    Ret x -> (x,counts)
    Bind m f -> let (a,counts') = loop env counts m in loop env counts' (f a)
    Lookup x -> (maybe (error $ "lookup:"<>x) id (Map.lookup x env), counts)
    ModEnv f m -> loop (f env) counts m
    Save -> (env,counts)
    Restore env m -> loop env counts m
    CountApp -> ((), counts { apps = apps counts + 1 })
    CountAdd -> ((), counts { adds = adds counts + 1 })

type Env = Map Var Value

data Counts = Counts { apps :: Int, adds :: Int } deriving (Show)
