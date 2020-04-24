
module Cek2(evaluate) where -- close/inline the outer stepping-loop

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
data Control = CE Exp | CV Value deriving (Show)
data Kont {-k-}
  = Kdone
  | Karg Env Exp Kont
  | Kfun Value Kont
  | Kadd1 Env Exp Kont
  | Kadd2 Value Kont
  deriving (Show)

evaluate :: Exp -> Result
evaluate e = run (CE e, Map.empty, Kdone) counts0 where

  run m counts = do
    let counts' = bump m counts
    let continue m = run m counts'
    case m of
      (CV v, _, Kdone) -> Result v counts'

      (CE (Num n), q, k) -> continue (CV (Number n), q, k)
      (CE (Var x), q, k) -> continue (CV (look x q), q, k)
      (CE (Lam x body), q, k) -> continue (CV (Clo q x body), q, k)

      (CE (App e1 e2), q, k) -> continue (CE e1, q, Karg q e2 k)
      (CV v, _, Karg q e k) -> continue (CE e, q, Kfun v k)
      (CV _, _, Kfun Number{} _) -> error "cant apply a non-function"
      (CV v, _, Kfun (Clo q x e) k) -> continue (CE e, Map.insert x v q, k)

      (CE (Add e1 e2), q, k) -> continue (CE e1, q, Kadd1 q e2 k)
      (CV v1, _, Kadd1 q e k) -> continue (CE e, q, Kadd2 v1 k)
      (CV (Number n2), q, Kadd2 (Number n1) k) -> continue (CV (Number (n1+n2)), q, k)
      (CV _, _, Kadd2 _ _) -> error "cant add non-numbers"

      (CE (Let x rhs body), q, k) -> continue (CE (App (Lam x body) rhs), q, k)

----------------------------------------------------------------------

data Counts = Counts (Map Class Int)

instance Show Counts where
  show (Counts m) =
    unlines $ map (\(c,i) -> unwords ["-", show c, show i]) $ Map.toList m

counts0 :: Counts
counts0 = Counts Map.empty

bump :: Machine -> Counts -> Counts
bump cek (Counts m) = Counts (Map.insertWith (+) (classify cek) 1 m)

data Class
  -- At (CE exp)...
  = AtNum
  | AtAdd
  | AtVar
  | AtLam
  | AtApp
  | AtLet
  -- At (CE value)...
  | AtKdone
  | AtKarg
  | AtKfun
  | AtKadd1
  | AtKadd2
  deriving (Show,Eq,Ord)

classify :: Machine -> Class
classify = \case
  (CE e, _, _) -> classifyE e
  (CV _, _, k) -> classifyK k

classifyE :: Exp -> Class
classifyE = \case
  Num{} -> AtNum
  Add{} -> AtAdd
  Var{} -> AtVar
  Lam{} -> AtLam
  App{} -> AtApp
  Let{} -> AtLet

classifyK :: Kont -> Class
classifyK = \case
  Kdone{} -> AtKdone
  Karg{} -> AtKarg
  Kfun{} -> AtKfun
  Kadd1{} -> AtKadd1
  Kadd2{} -> AtKadd2
