
module Cek(evaluate) where
-- CEK Machine to evaluate expressions

import Data.Map (Map,insert)
import qualified Data.Map.Strict as Map
import Ast

data Result = Result Value Counts

instance Show Result where
  show (Result v counts) =
    unlines [ "value: " ++ show v, "counts:", show counts ]

data Value
  = Number Int
  | Boolean Bool
  | Closure Env Var Exp
  | Fixed Env Var Exp
  | Vprim0 Op
  | Vprim1 Op Value
  deriving (Show)


type Machine {-m-} = (Counts,Control,Env,Kont)
data Counts  {-i-} = Counts (Map Micro Int)
data Control {-c-} = CE Exp | CV Value deriving (Show)
type Env     {-q-} = Map Var Value
data Kont    {-k-}
  = Kdone
  | Kbind Env Var Exp Kont
  | Kif Env Exp Exp Kont
  | Karg Env Exp Kont
  | Kfun Value Kont
  | Kfix Value Kont
  | Kprim0 Env Op Exp Kont
  | Kprim1 Value Op Kont
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
  (i, CE (Prim op), q, k)                       -> (i, CV (Vprim0 op), q, k)
  (i, CE (SatPrim e1 op e2), q, k)              -> (i, CE e1, q, Kprim0 q op e2 k)
  (i, CE (Var x), q, k)                         -> (i, CV (look x q), q, k)
  (i, CE (Lam x body), q, k)                    -> (i, CV (Closure q x body), q, k)
  (i, CE (App e1 e2), q, k)                     -> (i, CE e1, q, Karg q e2 k)
  (i, CE (Let x rhs body), q, k)                -> (i, CE rhs, q, Kbind q x body k)
  (i, CE (Fix x body), q, k)                    -> (i, CV (Fixed q x body), q, k)
  (i, CE (Ite e1 e2 e3), q, k)                  -> (i, CE e1, q, Kif q e2 e3 k)

  (_, CV _, _, Kdone)                           -> error "stepping a finished machine"
  (i, CV v, _, Kbind q x e k)                   -> (i, CE e, insert x v q, k)

  (i, CV v, _, Kprim0 q op e k)                 -> (i, CE e, q, Kprim1 v op k)
  (i, CV v, q, Kprim1 v1 op k)                  -> (tick [DoComp op] i, CV (doOp op v1 v), q, k)

  (i, CV (Boolean True),  _, Kif q e _ k)       -> (i, CE e, q, k)
  (i, CV (Boolean False), _, Kif q _ e k)       -> (i, CE e, q, k)
  (_, CV _, _, Kif{})                           -> error "cant test a non-boolean"

  (i, CV v, _, Karg q e k)                      -> (i, CE e, q, Kfun v k)
  (_, CV _, _, Kfun Number{} _)                 -> error "cant apply a number"
  (_, CV _, _, Kfun Boolean{} _)                -> error "cant apply a boolean"
  (i, CV v, q, Kfun (Vprim0 op) k)              -> (i, CV (Vprim1 op v), q, k)
  (i, CV v, q, Kfun (Vprim1 op v1) k)           -> (tick [DoComp op] i, CV (doOp op v1 v), q, k)
  (i, CV v, _, Kfun (Closure q x e) k)          -> (i, CE e, insert x v q, k)
  (i, CV v, _, Kfun (fixed@(Fixed q x e)) k)    -> (i, CE e, insert x fixed q, Kfix v k)

  (i, CV (Closure q x e), _, Kfix v k)          -> (i, CE e, insert x v q, k)
  (_, CV _, _, Kfix{})                          -> error "cant fix a non-closure"


-- | The micro steps about to be taken, from a given machine state
micsM :: Machine -> [Micro]
micsM = \case
  (_, CE e, _, _) -> [DoControlE, microE e]
  (_, CV _, _, k) -> [DoControlV, microK k]


look :: Var -> Env -> Value
look x env = maybe (error $ "lookup:" ++ x) id (Map.lookup x env)

doOp :: Op -> Value -> Value -> Value
doOp = \case Add -> add; Sub -> sub; Mul -> mul; Leq -> leq

add,sub,mul,leq :: Value -> Value -> Value

add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"

sub (Number n1) (Number n2) = Number (n1-n2)
sub _ _ = error "can't sub non-numbers"

mul (Number n1) (Number n2) = Number (n1*n2)
mul _ _ = error "can't mul non-numbers"

leq (Number n1) (Number n2) = Boolean (n1 <= n2)
leq _ _ = error "can't leq non-numbers"


-- | The micro step about to be taken, from a machine control expression
microE :: Exp -> Micro
microE = \case
  Num{} -> DoNum
  SatPrim _ op _ -> DoComp op
  Prim op -> DoPrim op
  Var{} -> DoVar
  Lam{} -> DoLam
  App{} -> DoApp
  Let{} -> DoLet
  Fix{} -> DoFix
  Ite{} -> DoIte

-- | The micro step about to be taken, from a machine continuation
microK :: Kont -> Micro
microK = \case
  Kdone{} -> DoKdone
  Kbind{} -> DoKbind
  Kif{} -> DoKif
  Karg{} -> DoKarg
  Kfun{} -> DoKfun
  Kfix{} -> DoKfix
  Kprim0{} -> DoKprim0
  Kprim1{} -> DoKprim1


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
  | DoPrim Op
  | DoVar
  | DoLam
  | DoApp
  | DoLet
  | DoFix
  | DoIte
  -- Dispatch on continuation (when control is a value)
  | DoKdone
  | DoKbind
  | DoKif
  | DoKarg
  | DoKfun
  | DoKfix
  | DoKprim0
  | DoKprim1
  -- Useful computation
  | DoComp Op
  deriving (Show,Eq,Ord)

