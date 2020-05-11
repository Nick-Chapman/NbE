
module Anf(flatten, Var, Code(..), Atom(..)) where
-- ANF code: flattened expressions
-- Compiler("flatten") from Ast

import Control.Monad(ap,liftM)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Ast

----------------------------------------------------------------------
-- Flattened (ANF style) code

-- TODO: support for AddOp, and for compiling it to saturated form when possible

data Atom = AVar Var | ANum Int

data Code -- flattened expression
  = Return Atom
  | Tail Var Atom
  | LetCode Var Code Code
  | LetOp Var Op (Atom,Atom) Code
  | LetLam Var (Var,Code) Code
  | LetFix Var (Var,Code) Code
  | Branch Atom Code Code

instance Show Atom where show = \case ANum n -> show n; AVar s -> s
instance Show Code where show = unlines . pretty

-- | pretty print, showing explicit continutaion management: push, return, (and tail)
pretty :: Code -> [String]
pretty = \case
  Return a -> ["return: " ++ show a]
  Tail xf a -> ["tail: " ++ xf ++ " " ++ show a]
  --LetCode x rhs body -> indented ("let " ++ x ++ " =") (pretty rhs) ++ pretty body
  LetCode x rhs body -> indented ("push: " ++ x ++  " ->") (pretty body) ++ pretty rhs
  LetOp x op (a1,a2) c -> indented ("let " ++ x ++ " =") [show (op,a1,a2)] ++ pretty c
  LetLam y (x,body) c -> indented ("let " ++ y ++ " = \\" ++ x ++ ".") (pretty body) ++ pretty c
  LetFix f (x,body) c -> indented ("letrec " ++ f ++ " = \\" ++ x ++ ".") (pretty body) ++ pretty c
  Branch a1 c2 c3 -> ["if " ++ show a1] ++ indented "then" (pretty c2) ++ indented "else" (pretty c3)

indented :: String -> [String] -> [String]
indented hang = \case
  [] -> error "indented, no body"
  [oneLine] -> [hang ++ " " ++ oneLine]
  lines -> [hang] ++ ["  " ++ line | line <- lines]


----------------------------------------------------------------------
-- compile time

-- | compile an expression to (flat)code for a CEK machine
flatten :: Exp -> Code
flatten exp = runM (codifyAs Nothing exp)

codifyAs :: Maybe Var -> Exp -> M Code
codifyAs mx = \case
  Num n -> do
    return $ Return $ ANum n
  Prim op -> do
    codePrim op
  SatPrim e1 op e2 -> do
    a1 <- atomize $ codify e1
    a2 <- atomize $ codify e2
    name <- fresh mx
    Wrap (LetOp name op (a1,a2)) (return $ Return $ AVar name)
  Var x -> do
    a <- Lookup x
    return $ Return a
  Lam formal body -> do
    let bodyName = fmap (++"-body") mx
    name <- fresh mx
    body <- ModEnv (Map.insert formal (AVar formal)) $ Reset (codifyAs bodyName body)
    Wrap (LetLam name (formal,body)) (return $ Return $ AVar name)
  App func arg -> do
    aFunc <- atomize $ Reset (codify func) -- why reset?
    --aArg <- atomize $ Reset (codify arg)
    aArg <- atomize $ codify arg
    case aFunc of
      ANum{} -> error "application of number detected"
      AVar f -> return $ Tail f aArg
  Let x rhs body -> do
    a <- atomizeAs (Just x) $ codifyAs (Just x) rhs
    ModEnv (Map.insert x a) $ codifyAs mx body
  Fix f (Lam x body) -> do
    let mod = Map.insert f (AVar f) . Map.insert x (AVar x)
    body <- ModEnv mod $ Reset (codify body)
    Wrap (LetFix f (x,body)) (return $ Return $ AVar f)
  Fix{} -> error "fix of non-lambda detected"
  Ite e1 e2 e3 -> do
    let thenName = fmap (++"-then") mx
    let elseName = fmap (++"-else") mx
    a1 <- atomize $ codify e1
    c2 <- Reset (codifyAs thenName e2)
    c3 <- Reset (codifyAs elseName e3)
    return $ Branch a1 c2 c3
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

codePrim :: Op -> M Code
codePrim op = do
  xAdd <- Fresh
  xAdd1 <- Fresh
  x1 <- Fresh
  x2 <- Fresh
  xRes <- Fresh
  return $ LetLam xAdd (x1,LetLam xAdd1 (x2, LetOp xRes op (AVar x1,AVar x2) (Return (AVar xRes))) (Return (AVar xAdd1))) (Return (AVar xAdd))


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
