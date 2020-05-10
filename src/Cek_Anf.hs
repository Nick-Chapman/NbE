
module Cek_Anf(execute) where
-- (CEK style) Machine to execute ANF code

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast
import qualified Anf (Code(..),Atom(..))

data Result = Result Value Counts

instance Show Result where
  show (Result v counts) =
    unlines [ "value: " ++ show v, "counts:", show counts ]

data Value = Number Int | Closure Env Var Anf.Code

instance Show Value where show = \case Number n -> show n; Closure{} -> "<closure>"

----------------------------------------------------------------------
-- run time

type Machine {-m-} = (Counts,Control,Env,Kont)
data Counts  {-i-} = Counts (Map Micro Int)
type Control {-c-} = Anf.Code
type Env     {-q-} = Map Var Value
data Kont    {-k-} = Kbind Env Var Anf.Code Kont | Kdone


-- | execute (flat)code on a CEK machine
execute :: Anf.Code -> Result
execute = run . install

-- | initialize a machine with the (flat)code to execute
install :: Anf.Code -> Machine
install c = (counts0, c, Map.empty, Kdone)

-- | run a machine unti the final value is calculated
run :: Machine -> Result
run (i,c,q,k) = case c of
  Anf.Return a -> ret i (atomic q a) k
  Anf.Tail f a -> enter (tick [DoEnter] i) (look f q) (atomic q a) k
  Anf.LetAdd x (a1,a2) c -> run (tick [DoAddition] i, c, q', k) where q' = Map.insert x (add (atomic q a1) (atomic q a2)) q
  Anf.LetLam x (fx,fc) c -> run (tick [DoMakeClosure] i, c, q', k) where q' = Map.insert x (Closure q fx fc) q
  Anf.LetCode x rhs body -> run (tick [DoPushContinuation] i, rhs, q, Kbind q x body k)

ret :: Counts -> Value -> Kont -> Result
ret i v = \case
  Kdone -> Result v i'
  Kbind q x c k -> run (i', c,q',k) where q' = Map.insert x v q
  where i' = tick [DoReturn] i

enter :: Counts -> Value -> Value -> Kont -> Result
enter i func arg k = case func of
  Number{} -> error "cant enter a number"
  Closure q formal body -> run (i, body, q', k) where q' = Map.insert formal arg q

atomic :: Env -> Anf.Atom -> Value
atomic q = \case
  Anf.AVar x -> look x q
  Anf.ANum n -> Number n

look :: Var -> Env -> Value
look x q = maybe (error $ "runtime-lookup:" ++ x) id (Map.lookup x q)

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"


----------------------------------------------------------------------
-- instrumentation

tick :: [Micro] -> Counts -> Counts
tick mics i = foldl countMicro i mics

instance Show Counts where
  show (Counts m) =
    unlines $ map (\(c,i) -> unwords ["-", show c, show i]) $ Map.toList m

counts0 :: Counts
counts0 = Counts Map.empty

countMicro :: Counts -> Micro -> Counts
countMicro (Counts mm) cl = Counts (Map.insertWith (+) cl 1 mm)

data Micro
  = DoReturn
  | DoEnter
  | DoPushContinuation
  | DoAddition
  | DoMakeClosure
  deriving (Show,Eq,Ord)
