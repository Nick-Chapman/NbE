{-# LANGUAGE RecursiveDo #-}

module Evaluate(evaluate,Counts(..)) where
-- Standard expression evaluator, with instrumentation

import Control.Monad(ap,liftM)
import Control.Monad.Fix (MonadFix,mfix)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

evaluate :: Exp -> (Value,Counts)
evaluate exp = runM (eval exp)

data Value
  = Number Int
  | Boolean Bool
  | Function Fun
  | VPrim0 Op
  | VPrim1 Op Value
  deriving (Show)

newtype Fun = Fun (Value -> M Value)
instance Show Fun where show _ = "<Fun>"

eval :: Exp -> M Value
eval = \case
  Num n -> do
    return $ Number n
  Prim op ->
    return $ VPrim0 op
  SatPrim e1 op e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    doOp op v1 v2
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
  Fix f body -> mdo
    fixed <- ModEnv (Map.insert f fixed) $ eval body
    return fixed
  Ite i t e -> do
    i <- eval i
    ite i t e

ite :: Value -> Exp -> Exp -> M Value
ite = \case
  Boolean True -> \t _ -> eval t
  Boolean False -> \_ e -> eval e
  _ -> error "test of an if-then-else  must be boolean"

apply :: Value -> Value -> M Value
apply = \case
  Function (Fun f) -> \arg -> f arg
  VPrim0 op -> \arg -> return $ VPrim1 op arg
  VPrim1 op v1 -> \v2 -> doOp op v1 v2
  f@Number{} -> error $ "can't apply a number: " <> show f
  f@Boolean{} -> error $ "can't apply a boolean: " <> show f


doOp :: Op -> Value -> Value -> M Value
doOp op v1 v2 = case op of
  Add -> do CountAdd; return $ add v1 v2
  Sub -> do CountSub; return $ sub v1 v2
  Mul -> do CountMul; return $ mul v1 v2
  Leq -> do CountLeq; return $ leq v1 v2

add,sub,mul,leq :: Value -> Value -> Value

add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"

sub (Number n1) (Number n2) = Number (n1-n2)
sub _ _ = error "can't sub non-numbers"

mul (Number n1) (Number n2) = Number (n1*n2)
mul _ _ = error "can't mul non-numbers"

leq (Number n1) (Number n2) = Boolean (n1 <= n2)
leq _ _ = error "can't leq non-numbers"


instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind
instance MonadFix M where mfix = MFix

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  MFix :: (a -> M a) -> M a
  Lookup :: Var -> M Value
  ModEnv :: (Env -> Env) -> M a -> M a
  Save :: M Env
  Restore :: Env -> M a -> M a
  CountApp :: M ()
  CountAdd :: M ()
  CountSub :: M ()
  CountMul :: M ()
  CountLeq :: M ()

runM :: M Value -> (Value,Counts)
runM m = loop Map.empty c0 m where
  c0 = Counts 0 0 0 0 0
  loop :: Env -> Counts -> M a -> (a,Counts)
  loop env counts = \case
    Ret x -> (x,counts)
    Bind m f -> let (a,counts') = loop env counts m in loop env counts' (f a)
    MFix f -> do
      let x@(a,_) = loop env counts (f a)
      x
    Lookup x -> (maybe (error $ "lookup:"<>x) id (Map.lookup x env), counts)
    ModEnv f m -> loop (f env) counts m
    Save -> (env,counts)
    Restore env m -> loop env counts m
    CountApp -> ((), counts { apps = apps counts + 1 })
    CountAdd -> ((), counts { adds = adds counts + 1 })
    CountSub -> ((), counts { subs = subs counts + 1 })
    CountMul -> ((), counts { muls = muls counts + 1 })
    CountLeq -> ((), counts { leqs = leqs counts + 1 })

type Env = Map Var Value

data Counts = Counts
  { apps :: Int
  , adds :: Int
  , subs :: Int
  , muls :: Int
  , leqs :: Int
  } deriving (Show)
