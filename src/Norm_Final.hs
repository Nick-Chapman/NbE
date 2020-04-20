
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
  = Syntax Exp
  | Macro (SemVal -> M SemVal)

reify :: SemVal -> M Exp
reify = \case
  Syntax e -> return e
  Macro f -> do
    x <- Fresh
    body <- Reset (f (Syntax $ Var x) >>= reify)
    return $ Lam x body

nameIt :: Exp -> M Var
nameIt exp = do
  x <- Fresh
  Wrap (Let x exp) (return x)

apply :: SemVal -> SemVal -> M SemVal
apply = \case
  Syntax e ->
    \arg -> do
      arg <- reify arg
      return $ Syntax (App e arg)
  Macro f ->
    \arg ->
      if duplicatable arg then f arg else do
        arg <- reify arg
        x <- nameIt arg
        f (Syntax (Var x))

duplicatable :: SemVal -> Bool
duplicatable = \case
  Macro{} -> True
  Syntax (Var{}) -> True
  Syntax _ -> False

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
  Add e1 e2 -> do
    e1 <- norm e1
    e2 <- norm e2
    return $ Syntax $ Add e1 e2
  Num n -> do
    return $ Syntax (Num n)

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
  Wrap :: (Exp -> Exp) -> M a -> M a

runM :: M Exp -> Exp
runM m = snd $ loop Map.empty 1 m k0 where
  k0 s e = (s,e)
  loop :: Env -> State -> M a -> (State -> a -> Res) -> Res
  loop env state m k = case m of

    Ret x -> k state x
    Bind m f -> loop env state m $ \state a -> loop env state (f a) k
    Lookup x -> k state $ fromJust $ Map.lookup x env
    Save -> k state env
    Restore env m -> loop env state m k
    ModEnv f m -> loop (f env) state m k
    Fresh -> k (state+1) ("v" <> show state)
    Reset m -> let (state',v) = loop env state m k0 in k state' v
    Wrap f m -> f' (loop env state m k) where f' (s,e) = (s,f e)

type Res = (State,Exp)
type Env = Map Var SemVal
type State = Int
