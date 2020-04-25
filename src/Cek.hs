
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
evaluate e = loop (install e) where
  loop :: Machine -> Result
  loop m = do
    case finished m of
      Just res -> res
      Nothing -> loop (step (tick [DoOuterStep] m))

install :: Exp -> Machine
install e = (counts0, CE e, Map.empty, Kdone)

finished :: Machine -> Maybe Result
finished = \case (i, CV v, _, Kdone) -> Just (Result v i); _ -> Nothing

type Machine {-m-} = (Counts,Control,Env,Kont)
data Counts  {-i-} = Counts (Map Micro Int)
data Control {-c-} = CE Exp | CV Value deriving (Show)
type Env     {-q-} = Map Var Value
data Kont    {-k-}
  = Kdone
  | Karg Env Exp Kont
  | Kfun Value Kont
  | Kadd1 Env Exp Kont
  | Kadd2 Value Kont
  | Kbind Env Var Exp Kont
  deriving (Show)

-- | take one step of the machine; the machine must not be in a finished state
step :: Machine -> Machine
step m = case tick (micsM m) m of
  (i, CE (Num n), q, k)                 -> (i, CV (Number n), q, k)
  (i, CE (Add e1 e2), q, k)             -> (i, CE e1, q, Kadd1 q e2 k)
  (i, CE (Var x), q, k)                 -> (i, CV (look x q), q, k)
  (i, CE (Lam x body), q, k)            -> tick [DoClose] (i, CV (Clo q x body), q, k)
  (i, CE (App e1 e2), q, k)             -> (i, CE e1, q, Karg q e2 k)
  (i, CE (Let x rhs body), q, k)        -> (i, CE rhs, q, Kbind q x body k)

  (_, CV _, _, Kdone)                   -> error "stepping a finished machine"
  (i, CV v, _, Karg q e k)              -> (i, CE e, q, Kfun v k)
  (_, CV _, _, Kfun Number{} _)         -> error "cant apply a non-function"
  (i, CV v, _, Kfun (Clo q x e) k)      -> (i, CE e, Map.insert x v q, k)
  (i, CV v1, _, Kadd1 q e k)            -> (i, CE e, q, Kadd2 v1 k)
  (i, CV v1, q, Kadd2 v2 k)             -> tick [DoAddition] (i, CV (add v1 v2), q, k)
  (i, CV v, _, Kbind q x e k)           -> (i, CE e, Map.insert x v q, k)


look :: Var -> Env -> Value
look x env = maybe (error $ "lookup:" ++ x) id (Map.lookup x env)

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"


-- | The micro steps about to be taken, from a given machine state
micsM :: Machine -> [Micro]
micsM = \case
  (_, CE e, _, _) -> [DoControlE, micsE e]
  (_, CV _, _, k) -> [DoControlV, micsK k]

micsE :: Exp -> Micro
micsE = \case
  Num{} -> DoNum
  Add{} -> DoAdd
  Var{} -> DoVar
  Lam{} -> DoLam
  App{} -> DoApp
  Let{} -> DoLet

micsK :: Kont -> Micro
micsK = \case
  Kdone{} -> DoKdone
  Karg{} -> DoKarg
  Kfun{} -> DoKfun
  Kadd1{} -> DoKadd1
  Kadd2{} -> DoKadd2
  Kbind{} -> DoKbind


-- | Record (in the machine state) a list of micro steps as having been taken
tick :: [Micro] -> Machine -> Machine
tick mics (i, c, q, k) = (foldl countMicro i mics, c, q, k)

instance Show Counts where
  show (Counts m) =
    unlines $ map (\(c,i) -> unwords ["-", show c, show i]) $ Map.toList m

counts0 :: Counts
counts0 = Counts Map.empty

countMicro :: Counts -> Micro -> Counts
countMicro (Counts mm) cl = Counts (Map.insertWith (+) cl 1 mm)

-- | Classification of _Micro_ steps taken by the machine
data Micro
  = DoOuterStep
  | DoControlE
  | DoControlV
  -- Dispatch on control expression
  | DoNum
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
  | DoClose
  | DoAddition
  deriving (Show,Eq,Ord)

