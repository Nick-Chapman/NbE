
module Anf(flatten, Code(..), Atom(..)) where
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
flatten :: Exp -> Code
flatten exp = runM (codifyAs Nothing exp)

codifyAs :: Maybe Var -> Exp -> M Code
codifyAs mx = \case
  Num n -> do
    return $ Return $ ANum n
  AddOp -> do
    codeAdd
  SaturatedAdd e1 e2 -> do
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

codeAdd :: M Code
codeAdd = do
  xAdd <- Fresh
  xAdd1 <- Fresh
  x1 <- Fresh
  x2 <- Fresh
  xRes <- Fresh
  return $ LetLam xAdd (x1,LetLam xAdd1 (x2, LetAdd xRes (AVar x1,AVar x2) (Return (AVar xRes))) (Return (AVar xAdd1))) (Return (AVar xAdd))


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
