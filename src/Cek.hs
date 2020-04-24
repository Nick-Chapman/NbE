
module Cek(evaluate) where

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

evaluate :: Exp -> Result
evaluate e = loop counts0 (inject e) where
  loop :: Counts -> Machine -> Result
  loop counts m = do
    case finished m of
      Just v -> Result v (bump m counts)
      Nothing -> loop (bump m counts) (step m)

inject :: Exp -> Machine
inject e = (CE e, Map.empty, Kdone)

finished :: Machine -> Maybe Value
finished = \case (CV v, _, Kdone) -> Just v; _ -> Nothing

look :: Var -> Env -> Value
look x env = maybe (error $ "lookup:" ++ x) id (Map.lookup x env)

type Machine = (Control,Env,Kont)
data Control = CE Exp | CV Value deriving (Show)
type Env {-q-} = Map Var Value
data Kont {-k-}
  = Kdone
  | Karg Env Exp Kont
  | Kfun Value Kont
  | Kadd1 Env Exp Kont
  | Kadd2 Value Kont
  | Kbind Var Exp Kont
  deriving (Show)

step :: Machine -> Machine
step = \case
  (CE (Num n), q, k) -> (CV (Number n), q, k)
  (CE (Var x), q, k) -> (CV (look x q), q, k)
  (CE (Lam x body), q, k) -> (CV (Clo q x body), q, k)

  (CV _, _, Kdone) -> error "stepping a finished machine"

  (CE (App e1 e2), q, k) -> (CE e1, q, Karg q e2 k)
  (CV v, _, Karg q e k) -> (CE e, q, Kfun v k)
  (CV _, _, Kfun Number{} _) -> error "cant apply a non-function"
  (CV v, _, Kfun (Clo q x e) k) ->  (CE e, Map.insert x v q, k)

  (CE (Add e1 e2), q, k) -> (CE e1, q, Kadd1 q e2 k)
  (CV v1, _, Kadd1 q e k) -> (CE e, q, Kadd2 v1 k)
  (CV (Number n2), q, Kadd2 (Number n1) k) -> (CV (Number (n1+n2)), q, k)
  (CV _, _, Kadd2 _ _) -> error "cant add non-numbers"

  --(CE (Let x rhs body), q, k) -> (CE (App (Lam x body) rhs), q, k)
  (CE (Let x rhs body), q, k) -> (CE rhs, q, Kbind x body k)
  (CV v, q, Kbind x e k) -> (CE e, Map.insert x v q, k)


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
  | AtKbind
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
  Kbind{} -> AtKbind
