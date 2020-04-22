
module Norm(normalize) where

import Control.Monad(ap,liftM,(>=>))
import Ast

normalize :: Exp -> Exp
normalize = runM . norm

norm :: Exp -> M Exp
norm = reflect >=> reify

data SemVal

reify :: SemVal -> M Exp
reify = undefined

reflect :: Exp -> M SemVal
reflect = \case
  Num{} -> do
    undefined
  Add{} -> do
    undefined
  Var{} -> do
    undefined
  Lam{} -> do
    undefined
  App{} -> do
    undefined
  Let x rhs body ->
    reflect (App (Lam x body) rhs)

runM :: M Exp -> Exp
runM = undefined

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
