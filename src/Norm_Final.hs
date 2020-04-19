
module Norm_Final(normalize) where

import Control.Monad(ap,liftM,(>=>))
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

import Ast

normalize :: Exp -> Exp
normalize = runM . norm

norm :: Exp -> M Exp
norm = reflect >=> reify

data SemVal
  = Variable Var
  | Macro (SemVal -> M SemVal)

reify :: SemVal -> M Exp
reify = \case
  Variable x -> return (Var x)
  Macro f -> do
    x <- Fresh
    body <- Reset (f (Variable x) >>= reify)
    return $ Lam x body

nameIt :: Exp -> M Var
nameIt exp = do
  x <- Fresh
  Shift $ \k -> do
    Let x exp (k x)

apply :: SemVal -> SemVal -> M SemVal
apply = \case
  Variable f ->
    \arg -> do
      arg <- reify arg
      x <- nameIt (App (Var f) arg)
      return $ Variable x
  Macro f ->
    \arg -> f arg

reflect :: Exp -> M SemVal
reflect = \case
  Var x -> Lookup x
  Lam x exp -> do
    env <- Save
    return $ Macro $ \arg -> do
      Restore env $ ModEnv (Map.insert x arg) $ reflect exp
  App e1 e2 -> do
    v1 <- reflect e1
    v2 <- reflect e2
    apply v1 v2
  Let x rhs body -> do
    reflect (App (Lam x body) rhs)
    --arg <- reflect rhs
    --ModEnv (Map.insert x arg) $ reflect body
  Add e1 e2 -> do
    e1 <- norm e1
    e2 <- norm e2
    x <- nameIt (Add e1 e2)
    return $ Variable x
  Num n -> do
    x <- nameIt (Num n)
    return $ Variable x

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Lookup :: Var -> M SemVal
  ModEnv :: (Env -> Env) -> M a -> M a
  Save :: M Env
  Restore :: Env -> M a -> M a
  Fresh :: M Var
  Reset :: M Exp -> M Exp
  Shift :: ((a -> Exp) -> Exp) -> M a

runM :: M Exp -> Exp
runM m = loop Map.empty 1 m k0 where
  k0 _ e = e
  loop :: Env -> State -> M a -> (State -> a -> Exp) -> Exp
  loop env state m k = case m of

    Ret x -> k state x
    Bind m f -> loop env state m $ \state a -> loop env state (f a) k
    Lookup x -> k state $ fromJust $ Map.lookup x env
    Save -> k state env
    Restore env m -> loop env state m k
    ModEnv f m -> loop (f env) state m k
    Fresh -> k (state+1) ("_v" <> show state)
    Reset m -> k state (loop env state m k0)
    Shift f -> f (k state)

type Env = Map Var SemVal
type State = Int
