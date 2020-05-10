
module Cek(evaluate) where
-- CEK Machine to evaluate expressions

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

data Result = Result Value Counts

instance Show Result where
  show (Result v counts) =
    unlines [ "value: " ++ show v, "counts:", show counts ]

data Value = Number Int | Clo Closure | Vadd0 | Vadd1 Value deriving (Show)
data Closure = Closure Env Var Exp deriving (Show)


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


evaluate :: Exp -> Result
evaluate e = loop (install e) where
  loop :: Machine -> Result
  loop m@(i,c,q,k) = do
    case finished m of
      Just res -> res
      Nothing -> loop (step (tick [DoOuterStep] i,c,q,k))

install :: Exp -> Machine
install e = (counts0, CE e, Map.empty, Kdone)

finished :: Machine -> Maybe Result
finished = \case (i, CV v, _, Kdone) -> Just (Result v (tick [DoKdone] i)); _ -> Nothing


-- | take one step of the machine; the machine must not be in a finished state
step :: Machine -> Machine
step m@(i,e,q,k) = step' (tick (micsM m) i,e,q,k)

step' :: Machine -> Machine
step' = \case
  (i, CE (Num n), q, k)                         -> (i, CV (Number n), q, k)
  (i, CE (SaturatedAdd e1 e2), q, k)            -> (i, CE e1, q, Kadd1 q e2 k)
  (i, CE AddOp, q, k)                           -> (i, CV Vadd0, q, k)
  (i, CE (Var x), q, k)                         -> (i, CV (look x q), q, k)
  (i, CE (Lam x body), q, k)                    -> (i, CV (Clo (Closure q x body)), q, k)
  (i, CE (App e1 e2), q, k)                     -> (i, CE e1, q, Karg q e2 k)
  (i, CE (Let x rhs body), q, k)                -> (i, CE rhs, q, Kbind q x body k)

  (_, CV _, _, Kdone)                           -> error "stepping a finished machine"
  (i, CV v, _, Karg q e k)                      -> (i, CE e, q, Kfun v k)
  (_, CV _, _, Kfun Number{} _)                 -> error "cant apply a non-function"
  (i, CV v, _, Kfun (Clo (Closure q x e)) k)    -> (i, CE e, Map.insert x v q, k)
  (i, CV v, q, Kfun (Vadd0{}) k)                -> (i, CV (Vadd1 v), q, k)
  (i, CV v2, q, Kfun (Vadd1 v1) k)              -> (tick [DoAddition] i, CV (add v1 v2), q, k)
  (i, CV v1, _, Kadd1 q e k)                    -> (i, CE e, q, Kadd2 v1 k)
  (i, CV v2, q, Kadd2 v1 k)                     -> (tick [DoAddition] i, CV (add v1 v2), q, k)
  (i, CV v, _, Kbind q x e k)                   -> (i, CE e, Map.insert x v q, k)


-- | The micro steps about to be taken, from a given machine state
micsM :: Machine -> [Micro]
micsM = \case
  (_, CE e, _, _) -> [DoControlE, microE e]
  (_, CV _, _, k) -> [DoControlV, microK k]


look :: Var -> Env -> Value
look x env = maybe (error $ "lookup:" ++ x) id (Map.lookup x env)

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"


-- | The micro step about to be taken, from a machine control expression
microE :: Exp -> Micro
microE = \case
  Num{} -> DoNum
  SaturatedAdd{} -> DoSaturatedAdd
  AddOp{} -> DoAddOp
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
  = DoOuterStep
  | DoControlE
  | DoControlV
  -- Dispatch on control expression
  | DoNum
  | DoSaturatedAdd
  | DoAddOp
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

