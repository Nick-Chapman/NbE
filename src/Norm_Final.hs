
module Norm_Final(normalize) where

import Control.Monad(ap,liftM,(>=>))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

normalize :: Exp -> Exp
normalize = runM . norm

norm :: Exp -> M Exp
norm = reflect >=> reify

data SemVal = Syntax Exp | Macro (SemVal -> M SemVal)

reify :: SemVal -> M Exp
reify = \case
  Syntax e -> return e
  Macro f -> do
    x <- Fresh
    body <- Reset (f (Syntax (Var x)) >>= reify)
    return $ Lam x body

apply :: SemVal -> SemVal -> M SemVal
apply func arg = case func of
  Syntax func -> do
    arg <- reify arg
    return $ Syntax $ App func arg
  Macro f -> do
    if duplicatable arg then f arg else do -- beta!
      x <- Fresh
      arg <- reify arg
      Wrap (Let x arg) (f (Syntax (Var x)))

duplicatable :: SemVal -> Bool
duplicatable = \case
  Syntax (Var _) -> True
  Syntax _ -> False
  Macro{} -> True

reflect :: Exp -> M SemVal
reflect = \case

  Num n -> do
    return $ Syntax (Num n)

  AddOp -> do
    return $ Syntax AddOp

  SaturatedAdd e1 e2 -> do
    e1 <- norm e1
    e2 <- norm e2
    return $ Syntax (SaturatedAdd e1 e2)

  Var x -> do
    Lookup x

  Lam x body -> do
    env <- Save
    return $ Macro $ \arg ->
      Restore env $ ModEnv (Map.insert x arg) $ reflect body

  App e1 e2 -> do
    v1 <- reflect e1
    v2 <- reflect e2
    apply v1 v2

  Let x rhs body ->
    reflect (App (Lam x body) rhs)


instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Lookup :: Var -> M SemVal
  ModEnv :: (Env -> Env) -> M a -> M a
  Fresh :: M Var
  Save :: M Env
  Restore :: Env -> M a -> M a
  Wrap :: (Exp -> Exp) -> M a -> M a
  Reset :: M Exp -> M Exp

type Env = Map Var SemVal


runM :: M Exp -> Exp
runM m = snd $ loop Map.empty 1 m k0 where
  k0 s e = (s,e)
  loop :: Env -> State -> M a -> (State -> a -> Res) -> Res
  loop env state m k = case m of

    Ret x -> k state x
    Bind m f -> loop env state m $ \state a -> loop env state (f a) k
    Lookup x -> maybe (error $ "lookup:"<>x) (k state) (Map.lookup x env)
    Fresh -> k (state+1) ("v" <> show state)
    Save -> k state env
    Restore env m -> loop env state m k
    ModEnv f m -> loop (f env) state m k
    Wrap f m -> f' (loop env state m k) where f' (s,e) = (s,f e)
    Reset m -> let (state',v) = loop env state m k0 in k state' v

type Res = (State,Exp)
type State = Int
