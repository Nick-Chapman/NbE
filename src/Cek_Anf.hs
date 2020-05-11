
module Cek_Anf(execute) where
-- (CEK style) Machine to execute ANF code

import Data.Map (Map,insert)
import qualified Data.Map.Strict as Map
import Ast
import qualified Anf (Code(..),Atom(..))

data Result = Result Value Counts

instance Show Result where
  show (Result v counts) =
    unlines [ "value: " ++ show v, "counts:", show counts ]

data Value
  = Number Int
  | Boolean Bool
  | Closure Env Var Anf.Code
  | Fixed Env Var Var Anf.Code

instance Show Value where
  show = \case
    Number n -> show n
    Boolean b -> show b
    Closure{} -> "<closure>"
    Fixed{} -> "<fixed>"

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
  Anf.LetCode x rhs body -> run (tick [DoPushContinuation] i, rhs, q, Kbind q x body k)
  Anf.LetOp x op (a1,a2) c -> run (tick [DoPrim op] i, c, q', k) where q' = insert x (doOp op (atomic q a1) (atomic q a2)) q
  Anf.LetLam x (fx,fc) c -> run (tick [DoMakeClosure] i, c, q', k) where q' = insert x (Closure q fx fc) q
  Anf.LetFix f (x,body) c -> run (tick [DoMakeFixed] i, c, q', k) where q' = insert f (Fixed q f x body) q
  Anf.Branch a1 c2 c3 -> run (tick [DoBranch] i, branch c2 c3 (atomic q a1), q, k)

ret :: Counts -> Value -> Kont -> Result
ret i v = \case
  Kdone -> Result v i'
  Kbind q x c k -> run (i', c,q',k) where q' = insert x v q
  where i' = tick [DoReturn] i

enter :: Counts -> Value -> Value -> Kont -> Result
enter i func arg k = case func of
  Number{} -> error "cant enter a number"
  Boolean{} -> error "cant enter a boolean"
  Closure q x body -> run (i, body, q', k) where q' = insert x arg q
  me@(Fixed q f x body) -> run (i, body, q', k) where q' = insert f me (insert x arg q)


branch :: Anf.Code -> Anf.Code -> Value -> Anf.Code
branch c2 c3 = \case
  Boolean True -> c2
  Boolean False -> c3
  _ -> error "cant branch on a non boolean"

atomic :: Env -> Anf.Atom -> Value
atomic q = \case
  Anf.AVar x -> look x q
  Anf.ANum n -> Number n

look :: Var -> Env -> Value
look x q = maybe (error $ "runtime-lookup:" ++ x) id (Map.lookup x q)

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
  | DoPrim Op
  | DoMakeClosure
  | DoMakeFixed
  | DoBranch
  deriving (Show,Eq,Ord)
