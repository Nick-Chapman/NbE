
module Norm(normalize) where

import Control.Monad(ap,liftM,(>=>))
import Ast


normalize :: Exp -> Exp
normalize = runM . norm


norm :: Exp -> M Exp
norm = reflect >=> reify


data SemVal = Syntax Exp


reify :: SemVal -> M Exp
reify = \case
  Syntax e -> return e


reflect :: Exp -> M SemVal
reflect = \case
  Num n -> do
    return $ Syntax (Num n)
  Add e1 e2 -> do
    e1 <- norm e1
    e2 <- norm e2
    return $ Syntax $ Add e1 e2
  e@Var{} -> do
    return $ Syntax e
  Lam x body -> do
    return $ Syntax $ Lam x body
  App e1 e2 -> do
    e1 <- norm e1
    e2 <- norm e2
    return $ Syntax $ App e1 e2
  Let x rhs body ->
    reflect (App (Lam x body) rhs)


instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b

runM :: M Exp -> Exp
runM = undefined
