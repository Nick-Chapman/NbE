
module ClosureConvert(convert,execute) where
-- Closure-converted-ANF code (CC code)
-- Compiler("convert") from plain ANF
-- Machine execution of CC code

import Control.Monad(ap,liftM)
import Data.Map (Map)
import Data.Set (Set,(\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Ast
import qualified Anf(Code(..),Atom(..))

data Result = Result Value

instance Show Result where show (Result v) = unlines [ "value: " ++ show v ]

data Value
  = Number Int
  | Closure {fvs :: [Value], arity :: Int, body :: Code }

instance Show Value where show = \case Number n -> show n; Closure{} -> "<closure>"

----------------------------------------------------------------------
-- Closure-Converted Code (CC code)

data Loc = LocArg Int | LocFree Int

data Atom = ALoc Loc | ANum Int

data Code
  = Return Atom
  | Tail Loc [Atom]
  | LetAdd (Atom,Atom) Code
  | LetLam { free :: [Loc], arity :: Int, body :: Code, code :: Code }
  | LetCode { free :: [Loc], rhs :: Code, follow :: Code }


----------------------------------------------------------------------
-- pretty print CC code

instance Show Loc where
  show = \case
    LocArg n -> "*" ++ show n
    LocFree n -> "~" ++ show n

instance Show Atom where show = \case ANum n -> show n; ALoc loc -> show loc
instance Show Code where show = unlines . pretty

pretty :: Code -> [String]
pretty = \case
  Return a -> ["return: " ++ show a]
  Tail func args -> ["tail: " ++ show func ++ " " ++ show args]
  LetCode{free,rhs,follow} ->
    indented ("push-k: " ++ show free ++  " ->") (pretty follow) ++ pretty rhs
  LetAdd (a1,a2) c ->
    ("let-add: " ++ show a1 ++ " + " ++ show a2) : pretty c
  LetLam{free,arity,body,code} ->
    indented ("let-lam: " ++ show free ++ " \\" ++ show arity ++ ".") (pretty body) ++ pretty code

indented :: String -> [String] -> [String]
indented hang = \case
  [] -> error "indented, no body"
  [oneLine] -> [hang ++ " " ++ oneLine]
  lines -> [hang] ++ ["  " ++ line | line <- lines]


----------------------------------------------------------------------
-- convert Anf to (Closure converted) Code
-- TODO: take advantage of multi apps & multi lambdas

convert :: Anf.Code -> Code
convert anf = runM (convertAnf anf)

convertAnf :: Anf.Code -> M Code
convertAnf = \case
  Anf.Return a -> Return <$> convertAtom a

  Anf.Tail func arg -> do
    func <- Lookup func
    arg <- convertAtom arg
    return $ Tail func [arg]

  Anf.LetAdd x (a1,a2) code -> do
    a1 <- convertAtom a1
    a2 <- convertAtom a2
    code <- Extend x $ convertAnf code
    return $ LetAdd (a1,a2) code

  Anf.LetLam x (var,body) code -> do
    let fvs = Set.toList $ fvsBinding (var,body)
    free <- mapM Lookup fvs
    let arity = 1
    let locations = [ (y,LocFree i) | (y,i) <- zip fvs [0..] ]
    body <- Reset locations $ Extend var $ convertAnf body
    code <- Extend x $ convertAnf code
    return $ LetLam {free,arity,body,code}

  Anf.LetCode x rhs follow -> do
    let fvs = Set.toList $ fvsBinding (x,follow)
    free <- mapM Lookup fvs
    rhs <- convertAnf rhs
    let locations = [ (y,LocFree i) | (y,i) <- zip fvs [0..] ]
    follow <- Reset locations $ Extend x $ convertAnf follow
    return $ LetCode {free,rhs,follow}

convertAtom :: Anf.Atom -> M Atom
convertAtom = \case
  Anf.ANum n -> return $ ANum n
  Anf.AVar x -> ALoc <$> Lookup x


fvsBinding :: (Var,Anf.Code) -> Set Var
fvsBinding (var,code) = fvsCode code \\ Set.singleton var

fvsCode :: Anf.Code -> Set Var
fvsCode = \case
  Anf.Return a -> fvsAtom a
  Anf.Tail func arg -> Set.singleton func <> fvsAtom arg
  Anf.LetAdd x (a1,a2) code -> fvsAtom a1 <> fvsAtom a2 <> fvsBinding (x,code)
  Anf.LetLam x (var,body) code -> fvsBinding (var,body) <> fvsBinding (x,code)
  Anf.LetCode x rhs follow -> fvsCode rhs <> fvsBinding (x,follow)

fvsAtom :: Anf.Atom -> Set Var
fvsAtom = \case
  Anf.ANum _ -> Set.empty
  Anf.AVar x -> Set.singleton x


instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Lookup :: Var -> M Loc
  Extend :: Var -> M a -> M a
  Reset :: [(Var,Loc)] -> M a -> M a

runM :: M a -> a
runM = loop 0 Map.empty where
  loop :: Int -> LocEnv -> M a -> a
  loop d env = \case
    Ret x -> x
    Bind m f -> loop d env (f (loop d env m))
    Reset env m -> loop 0 (Map.fromList env) m
    Lookup x -> maybe (error $ "compile-time-lookup:"<>show(x,env)) (rel d) (Map.lookup x env)
    Extend x m ->
      loop (d+1) (Map.insert x (LocArg d) env) m

rel :: Int -> Loc -> Loc -- relativize a location to a stack depth
rel d = \case
  loc@LocFree{} -> loc
  LocArg n -> LocArg (d-n-1)

type LocEnv = Map Var Loc


----------------------------------------------------------------------
-- machine to execute the closure-converted-code

type Machine {-m-} = (Control,Frame,Kont)
type Control {-c-} = Code
data Frame   {-f-} = Frame { fvs :: [Value], args :: [Value] }
data Kont    {-k-} = Kdone | Kbind { fvs :: [Value], code :: Code, kont :: Kont }

execute :: Code -> Result
execute = run. install

install :: Code -> Machine
install code = (code,frame0,Kdone) where frame0 = Frame [] []

run :: Machine -> Result
run (code0,f,k) = case code0 of
  Return atom -> ret (atomic f atom) k
  Tail func args -> enter (locate f func) (map (atomic f) args) k
  LetAdd (a1,a2) code -> run (code, push (add (atomic f a1) (atomic f a2)) f, k)
  LetLam {free,arity,body,code} -> run (code,push clo f,k)
    where clo = Closure{fvs = map (locate f) free, arity, body}
  LetCode{free,rhs,follow}-> run (rhs,f,k')
    where k' = Kbind {fvs = map (locate f) free, code = follow, kont=k}

ret :: Value -> Kont -> Result
ret v = \case
  Kdone -> Result v
  Kbind {fvs,code,kont} -> run (code, Frame {fvs, args = [v]}, kont)

atomic :: Frame -> Atom -> Value
atomic f = \case
  ANum n -> Number n
  ALoc loc -> locate f loc

locate :: Frame -> Loc -> Value
locate Frame{fvs,args} = \case
  LocFree n -> nth "LocFree" fvs n
  LocArg n -> nth "LocArg" args n

push :: Value -> Frame -> Frame
push v f@Frame{args} = f { args = v : args }

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"

enter :: Value -> [Value] -> Kont -> Result
enter func args k = case func of
  Number{} -> error "cant enter a number"
  clo@Closure{fvs,arity,body}
    | arity == n -> do
        run (body, Frame {fvs,args}, k)
    | arity < n -> do
        let (myArgs,overArgs) = splitAt arity args
        let k' = makeOverAppK overArgs k
        run (body, Frame {fvs,args = myArgs}, k')
    | otherwise -> do
        ret (makePap nMissing clo args) k
    where
      nMissing = arity - n
      n = length args

makeOverAppK :: [Value] -> Kont -> Kont
makeOverAppK overArgs kont = Kbind {fvs=overArgs, code, kont}
  where
    code = Tail (LocArg 0) args
    args = [ ALoc (LocFree i) | i <- [0 .. length overArgs - 1] ]

makePap :: Int -> Value -> [Value] -> Value
makePap nMissing clo argsSoFar = clo2
  where
    clo2 = Closure {fvs = clo : argsSoFar, arity = nMissing, body}
    body = Tail (LocFree 0) args
    args =
      [ ALoc (LocFree i) | i <- [1 .. length argsSoFar] ] ++
      [ ALoc (LocArg i) | i <- [0 .. nMissing - 1] ]

nth :: Show a => String -> [a] -> Int -> a
nth tag xs i = if i >= length xs then error (show (tag,i,xs)) else xs !! i
