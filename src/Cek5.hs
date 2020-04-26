
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
  | Tail Var Atom
  | LetCode Var Code Code
  | LetAdd Var (Atom,Atom) Code
  | LetLam Var (Var,Code) Code

instance Show Atom where show = \case ANum n -> show n; AVar s -> s
instance Show Code where show = unlines . pretty

-- | basic pretty print, see nested lets as in a functional program
_pretty :: Code -> [String]
_pretty = \case
  Return a -> [show a]
  Tail xf a -> [xf ++ " " ++ show a]
  LetCode x rhs body -> indented ("let " ++ x ++ " =") (pretty rhs) ++ pretty body
  LetAdd x (a1,a2) c -> indented ("let " ++ x ++ " =") [show a1 ++ " + " ++ show a2] ++ pretty c
  LetLam x (xf,xc) c -> indented ("let " ++ x ++ " = \\" ++ xf ++ ".") (pretty xc) ++ pretty c

-- | pretty print, showing explicit continutaion management: push, return, (and tail)
pretty :: Code -> [String]
pretty = \case
  Return a -> ["return: " ++ show a]
  Tail xf a -> ["tail: " ++ xf ++ " " ++ show a]
  LetCode x rhs body -> indented ("push: " ++ x ++  " ->") (pretty body) ++ pretty rhs
  LetAdd x (a1,a2) c -> indented ("let " ++ x ++ " =") [show a1 ++ " + " ++ show a2] ++ pretty c
  LetLam x (xf,xc) c -> indented ("let " ++ x ++ " = \\" ++ xf ++ ".") (pretty xc) ++ pretty c


indented :: String -> [String] -> [String]
indented hang = \case
  [] -> error "indented, no body"
  [oneLine] -> [hang ++ " " ++ oneLine]
  lines -> [hang] ++ ["  " ++ line | line <- lines]


----------------------------------------------------------------------
-- compile time

-- | compile an expression to (flat)code for a CEK machine
compile :: Exp -> Code
compile exp = runM (codifyAs Nothing exp)

codifyAs :: Maybe Var -> Exp -> M Code
codifyAs mx = \case
  Num n -> do
    return $ Return $ ANum n
  Add e1 e2 -> do
    a1 <- atomize $ codify e1
    a2 <- atomize $ codify e2
    name <- fresh mx
    Wrap (LetAdd name (a1,a2)) $ return $ Return $ AVar name
  Var x -> do
    a <- Lookup x
    return $ Return a
  Lam formal body -> do
    let bodyName = fmap (++"-body") mx
    name <- fresh mx
    body <- ModEnv (Map.insert formal (AVar formal)) $ Reset (codifyAs bodyName body)
    Wrap (LetLam name (formal,body)) (return $ Return $ AVar name)
  App func arg -> do
    aFunc <- atomize $ Reset (codify func)
    aArg <- atomize $ Reset (codify arg)
    case aFunc of
      ANum{} -> error "application of number detected"
      AVar f -> return $ Tail f aArg
  Let x rhs body -> do
    a <- atomizeAs (Just x) $ codifyAs (Just x) rhs
    ModEnv (Map.insert x a) $ codifyAs mx body
  where
    codify = codifyAs Nothing
    atomize = atomizeAs Nothing

atomizeAs :: Maybe Var -> M Code -> M Atom
atomizeAs mx m = do
  m >>= \case
    Return a -> return a -- dont re-name at atom
    rhs -> do
      x <- fresh mx
      Wrap (letCode' x rhs) $ return $ AVar x

-- | Avoid pushing a continutaion which calls a known function
letCode' :: Var -> Code -> Code -> Code
letCode' x rhs body
  | Tail x' arg <- body, x==x', LetLam f def (Return (AVar f')) <- rhs, f==f' =
  {-
      push: \x. tail: x arg in
      let f = \<def> in
      return: f
  -->
      let f = \<def> in
      tail: f arg
  -}
      LetLam f def (Tail f arg)
  | otherwise =
      LetCode x rhs body

fresh :: Maybe Var -> M Var
fresh = \case
  Just var -> return var
  Nothing -> Fresh


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
  k0 state code = (state,code)
  loop :: CompileEnv -> State -> M a -> (State -> a -> Res) -> Res
  loop env state m k = case m of

    Ret x -> k state x
    Bind m f -> loop env state m $ \state a -> loop env state (f a) k
    Reset m -> let (state',code) = loop env state m k0 in k state' code
    Wrap f m -> f' (loop env state m k) where f' (s,a) = (s,f a)
    Lookup x -> maybe (error $ "compile-time-lookup:"<>x) (k state) (Map.lookup x env)
    ModEnv f m -> loop (f env) state m k
    Fresh -> k (state+1) ("_v" <> show state)

type Res = (State,Code)
type State = Int
type CompileEnv = Map Var Atom


----------------------------------------------------------------------
-- run time

type Machine {-m-} = (Counts,Control,Env,Kont)
data Counts  {-i-} = Counts (Map Micro Int)
type Control {-c-} = Code
type Env     {-q-} = Map Var Value
data Kont    {-k-} = Kbind Env Var Code Kont | Kdone


-- | execute (flat)code on a CEK machine
execute :: Code -> Result
execute = run . install

-- | initialize a machine with the (flat)code to execute
install :: Code -> Machine
install c = (counts0, c, Map.empty, Kdone)

-- | run a machine unti the final value is calculated
run :: Machine -> Result
run (i,c,q,k) = case c of
  Return a -> send (tick [DoReturn] i) (atomic q a) k
  Tail f a -> jump i q f a k
  LetCode x rhs body -> run (tick [DoPushContinuation] i, rhs, q, Kbind q x body k)
  LetAdd x (a1,a2) c -> run (tick [DoAddition] i, c, q', k) where q' = Map.insert x (add (atomic q a1) (atomic q a2)) q
  LetLam x (fx,fc) c -> run (tick [DoMakeClosure] i, c, q', k) where q' = Map.insert x (Clo (Closure q fx fc)) q

send :: Counts -> Value -> Kont -> Result
send i v = \case
  Kdone -> Result v i
  Kbind q x c k -> run (i, c,q',k) where q' = Map.insert x v q

atomic :: Env -> Atom -> Value
atomic q = \case
  AVar x -> look x q
  ANum n -> Number n

jump :: Counts -> Env -> Var -> Atom -> Kont -> Result
jump i q f a k = run (tick [DoJump] i, cf, q', k)
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
  | DoJump
  | DoPushContinuation
  | DoAddition
  | DoMakeClosure
  deriving (Show,Eq,Ord)
