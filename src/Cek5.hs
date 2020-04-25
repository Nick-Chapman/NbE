
module Cek5(evaluate,compile,execute) where
-- Goal: pre-stage to flatten expression to ANF style
-- 2 phase evaluation: compile/execute, using (flat)code to link the phases

import Control.Monad(ap,liftM)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

data Result = Result Value Counts

instance Show Result where
  show (Result v counts) =
    unlines [ "value: " ++ show v, "counts:", show counts ]

data Value = Number Int | Clo Closure deriving (Show)
data Closure = Closure Env Var Code deriving (Show)

-- | compile an expression to (flat)code, and execute on a CEK machine
evaluate :: Exp -> Result
evaluate = execute . compile

----------------------------------------------------------------------
-- compiled code

data Atom = AVar Var | ANum Int

data Code -- flattened expression
  = Return Atom
  | LetAdd Var (Atom,Atom) Code
  | LetLam Var (Var,Code) Code
  | LetCall Var (Var,Atom) Code
  | Tail Var Atom

instance Show Code where show = unlines . pretty
instance Show Atom where show = \case ANum n -> show n; AVar s -> s

pretty :: Code -> [String]
pretty = \case
  Return a -> [show a]
  LetAdd x (a1,a2) c -> prettyLet (x ++ " = ") [show a1 ++ "+" ++ show a2] ++ pretty c
  LetLam x (xf,xc) c -> prettyLet (x ++ " = \\" ++ xf ++ ".") (pretty xc) ++ pretty c
  LetCall x (xf,a) c -> prettyLet (x ++ " = ") [xf ++ " " ++ show a] ++ pretty c
  Tail xf a -> [xf ++ " " ++ show a]

prettyLet :: String -> [String] -> [String]
prettyLet hang = \case
  [] -> error "prettyLet, no body"
  [oneLine] -> ["let " ++ hang ++ oneLine ++ " in "]
  lines -> ["let " ++ hang] ++ ["  " ++ line | line <- lines ] ++ ["in"]

----------------------------------------------------------------------
-- compile time

-- | compile an expression to (flat)code for a CEK machine
compile :: Exp -> Code
compile exp = runM (codify exp)

codify :: Exp -> M Code
codify exp = do
  compileRT Nothing exp >>= \case
    RT'Return a -> return $ Return a
    RT'Tail fx a -> return $ Tail fx a

data RT = RT'Return Atom | RT'Tail Var Atom -- return or tail

getName :: Maybe Var -> M Var
getName = \case
  Just x -> return x
  Nothing -> Fresh

atomize :: Maybe Var -> Exp -> M Atom
atomize mx exp = do
  compileRT mx exp >>= \case
    RT'Return a -> return a
    RT'Tail fx a -> do
      name <- getName mx
      Wrap (LetCall name (fx,a)) (return $ AVar name)

compileRT :: Maybe Var -> Exp -> M RT
compileRT mx = \case
  Num n -> do
    return $ RT'Return $ ANum n
  Add e1 e2 -> do
    a1 <- atomize Nothing e1
    a2 <- atomize Nothing e2
    name <- getName mx
    Wrap (LetAdd name (a1,a2)) (return $ RT'Return $ AVar name)
  Var x -> do
    a <- Lookup x
    return $ RT'Return a
  Lam fx body -> do
    name <- getName mx
    fc <- Reset (ModEnv (Map.insert fx (AVar fx)) $ codify body)
    Wrap (LetLam name (fx,fc)) (return $ RT'Return $ AVar name)
  App e1 e2 -> do
    a1 <- atomize Nothing e1
    a2 <- atomize Nothing e2
    case a1 of
      ANum{} -> error "application of number detected"
      AVar xf -> return $ RT'Tail xf a2
  Let x rhs body -> do
    a <- atomize (Just x) rhs
    ModEnv (Map.insert x a) $ compileRT mx body


instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Fresh :: M Var
  Reset :: M Code -> M Code
  Wrap :: (Code -> Code) -> M a -> M a
  ModEnv :: (CompileEnv -> CompileEnv) -> M a -> M a
  Lookup :: Var -> M Atom

runM :: M Code -> Code
runM m = snd $ loop Map.empty 1 m k0 where
  k0 s e = (s,e)
  loop :: CompileEnv -> State -> M a -> (State -> a -> Res) -> Res
  loop env state m k = case m of

    Ret x -> k state x
    Bind m f -> loop env state m $ \state a -> loop env state (f a) k
    Reset m -> let (state',v) = loop env state m k0 in k state' v
    Wrap f m -> f' (loop env state m k) where f' (s,e) = (s,f e)
    Lookup x -> maybe (error $ "compile-time-lookup:"<>x) (k state) (Map.lookup x env)
    ModEnv f m -> loop (f env) state m k
    Fresh -> k (state+1) ("_v" <> show state)

type Res = (State,Code)
type State = Int
type CompileEnv = Map Var Atom

----------------------------------------------------------------------
-- run time

type Env {-q-} = Map Var Value

type Machine = (Counts,Control,Env,Kont)
type Control = Code
data Kont = Kbind Env Var Code Kont | Kdone

-- | execute (flat)code on a CEK machine
execute :: Code -> Result
execute = run . install

-- | initialize a machine with the (flat)code to execute
install :: Code -> Machine
install c = (counts0, c, Map.empty, Kdone)

-- | run a machine unti the final value is calculated
run :: Machine -> Result
run (i,c,q,k) = case c of
  Return a -> send (bump DoReturn i) (atomic q a) k
  LetAdd x (a1,a2) c -> run (bump DoAdd i, c, q', k) where q' = Map.insert x (add (atomic q a1) (atomic q a2)) q
  LetLam x (fx,fc) c -> run (bump DoMakeClosure i, c, q', k) where q' = Map.insert x (Clo (Closure q fx fc)) q
  LetCall x (f,a) c -> jump (bump DoPushContinuation i) q f a (Kbind q x c k)
  Tail f a -> jump i q f a k

send :: Counts -> Value -> Kont -> Result
send i v = \case
  Kdone -> Result v i
  Kbind q x c k -> run (i, c,q',k) where q' = Map.insert x v q

atomic :: Env -> Atom -> Value
atomic q = \case
  AVar x -> look x q
  ANum n -> Number n

jump :: Counts -> Env -> Var -> Atom -> Kont -> Result
jump i q f a k = run (bump DoJump i, cf, q', k)
  where
    Clo (Closure qf xf cf) = look f q -- will fail if not a Closure
    q' = Map.insert xf (atomic q a) qf

look :: Var -> Env -> Value
look x q = maybe (error $ "runtime-lookup:" ++ x) id (Map.lookup x q)

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"

----------------------------------------------------------------------
-- instrumentation

data Counts {-z-} = Counts (Map Class Int)

instance Show Counts where
  show (Counts m) =
    unlines $ map (\(c,i) -> unwords ["-", show c, show i]) $ Map.toList m

counts0 :: Counts
counts0 = Counts Map.empty

bump :: Class -> Counts -> Counts
bump cl (Counts m) = Counts (Map.insertWith (+) cl 1 m)

data Class
  = DoJump
  | DoReturn
  | DoAdd
  | DoPushContinuation
  | DoMakeClosure
  deriving (Show,Eq,Ord)
