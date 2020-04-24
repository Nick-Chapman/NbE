
module Cek4(evaluate) where
-- Goal: proper handling of Let, using Kbind continuation

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

data Result = Result Value Counts

instance Show Result where
  show (Result v counts) =
    unlines [ "value: " ++ show v, "counts:", show counts ]

data Value
  = Number Int
  | Clo Env Var Exp
  deriving (Show)

look :: Var -> Env -> Value
look x env = maybe (error $ "lookup:" ++ x) id (Map.lookup x env)

type Env {-q-} = Map Var Value

----------------------------------------------------------------------

type Machine = (Control,Env,Kont)
type Control = Exp
data Kont {-k-}
  = Kdone
  | Karg Env Exp Kont
  | Kfun Value Kont
  | Kadd1 Env Exp Kont
  | Kadd2 Value Kont
  | Kbind Var Exp Kont
  deriving (Show)

evaluate :: Exp -> Result
evaluate e = run counts0 (e, Map.empty, Kdone) where

  run :: Counts -> Machine -> Result
  run z m = run' (bump m z) m

  run' :: Counts -> Machine -> Result
  run' z = \case
--    (Let x rhs body, q, k) -> run z (App (Lam x body) rhs, q, k)
    (Let x rhs body, q, k) -> run z (rhs, q, Kbind x body k)
    (Num n, q, k) -> runV z (Number n) q k
    (Var x, q, k) -> runV z (look x q) q k
    (Lam x body, q, k) -> runV z (Clo q x body) q k
    (App e1 e2, q, k) -> run z (e1, q, Karg q e2 k)
    (Add e1 e2, q, k) -> run z (e1, q, Kadd1 q e2 k)

  runV :: Counts -> Value -> Env -> Kont -> Result
  runV z v q = \case
    Karg q e k -> run z (e, q, Kfun v k)
    Kfun Number{} _k -> error "cant apply a non-function"
    Kfun (Clo q x e) k -> run z (e, Map.insert x v q, k)
    Kadd1 q e k -> run z (e, q, Kadd2 v k)
    Kadd2 v2 k -> runV z (add v v2) q k
    Kdone -> Result v z
    Kbind x e k -> run z (e, Map.insert x v q, k)

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "add"

----------------------------------------------------------------------

data Counts {-z-} = Counts (Map Class Int)

instance Show Counts where
  show (Counts m) =
    unlines $ map (\(c,i) -> unwords ["-", show c, show i]) $ Map.toList m

counts0 :: Counts
counts0 = Counts Map.empty

bump :: Machine -> Counts -> Counts
bump cek (Counts m) = Counts (Map.insertWith (+) (classify cek) 1 m)

data Class
  = AtNum
  | AtAdd
  | AtVar
  | AtLam
  | AtApp
  | AtLet
  deriving (Show,Eq,Ord)

classify :: Machine -> Class
classify = \case
  (e, _, _) -> classifyE e

classifyE :: Exp -> Class
classifyE = \case
  Num{} -> AtNum
  Add{} -> AtAdd
  Var{} -> AtVar
  Lam{} -> AtLam
  App{} -> AtApp
  Let{} -> AtLet
