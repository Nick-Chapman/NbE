
module Cek4(evaluate) where
-- close/inline the outer stepping-loop
-- eliminate the CE/CV split, and associated micro-steps

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

data Result = Result Value Counts

instance Show Result where
  show (Result v counts) =
    unlines [ "value: " ++ show v, "counts:", show counts ]

data Value = Number Int | Clo Closure deriving (Show)
data Closure = Closure Env Var Exp deriving (Show)


type Machine {-m-} = (Counts,Control,Env,Kont)
data Counts  {-i-} = Counts (Map Micro Int)
type Control {-e-} = Exp
type Env     {-q-} = Map Var Value
data Kont    {-k-}
  = Kdone
  | Karg Env Exp Kont
  | Kfun Value Kont
  | Kadd1 Env Exp Kont
  | Kadd2 Value Kont
  | Kbind Env Var Exp Kont
  deriving (Show)


evaluate :: Exp -> Result
evaluate e0 = run (counts0, e0, Map.empty, Kdone) where

  run :: Machine -> Result
  run (i,e,q,k) = run' (tick [microE e] i, e, q, k)

  run' :: Machine -> Result
  run' = \case
    (i, Let x rhs body, q, k)           -> run (i, rhs, q, Kbind q x body k)
    (i, Num n, q, k)                    -> runV i (Number n) q k
    (i, Var x, q, k)                    -> runV i (look x q) q k
    (i, Lam x body, q, k)               -> runV i (Clo (Closure q x body)) q k
    (i, App e1 e2, q, k)                -> run (i, e1, q, Karg q e2 k)
    (i, Add e1 e2, q, k)                -> run (i, e1, q, Kadd1 q e2 k)

  runV :: Counts -> Value -> Env -> Kont -> Result
  runV i v q k = runV' (tick [microK k] i) v q k

  runV' :: Counts -> Value -> Env -> Kont -> Result
  runV' i v q = \case
    Kdone                               -> Result v i
    Karg q e k                          -> run (i, e, q, Kfun v k)
    Kfun Number{} _k                    -> error "cant apply a non-function"
    Kfun (Clo (Closure q x e)) k        -> run (i, e, Map.insert x v q, k)
    Kadd1 q e k                         -> run (i, e, q, Kadd2 v k)
    Kadd2 v2 k                          -> runV (tick [DoAddition] i) (add v v2) q k
    Kbind q x e k                       -> run (i, e, Map.insert x v q, k)


look :: Var -> Env -> Value
look x env = maybe (error $ "lookup:" ++ x) id (Map.lookup x env)

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"


-- | The micro step about to be taken, from a machine control expression
microE :: Exp -> Micro
microE = \case
  Num{} -> DoNum
  Add{} -> DoAdd
  Var{} -> DoVar
  Lam{} -> DoLam
  App{} -> DoApp
  Let{} -> DoLet

-- | The micro step about to be taken, from a machine continuation
microK :: Kont -> Micro
microK = \case
  Kdone{} -> DoKdone
  Karg{} -> DoKarg
  Kfun{} -> DoKfun
  Kadd1{} -> DoKadd1
  Kadd2{} -> DoKadd2
  Kbind{} -> DoKbind


-- | Record (in the counts) a list of micro steps as having been taken

tick :: [Micro] -> Counts -> Counts
tick mics i = foldl countMicro i mics

instance Show Counts where
  show (Counts m) =
    unlines $ map (\(c,i) -> unwords ["-", show c, show i]) $ Map.toList m

counts0 :: Counts
counts0 = Counts Map.empty

countMicro :: Counts -> Micro -> Counts
countMicro (Counts mm) cl = Counts (Map.insertWith (+) cl 1 mm)

-- | Classification of _Micro_ steps taken by the machine
data Micro
  -- Dispatch on control expression
  = DoNum
  | DoAdd
  | DoVar
  | DoLam
  | DoApp
  | DoLet
  -- Dispatch on continuation (when control is a value)
  | DoKdone
  | DoKarg
  | DoKfun
  | DoKadd1
  | DoKadd2
  | DoKbind
  -- Useful computation
  | DoAddition
  deriving (Show,Eq,Ord)
