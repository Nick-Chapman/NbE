
module ClosureConvert(convert,execute) where
-- Closure-converted-(multi)ANF code (CC code)
-- Compiler("convert") from plain-(multi)ANF
-- Machine execution of (multi)CC code

import Control.Monad(ap,liftM)
import Data.Map (Map)
import Data.Set (Set,(\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Ast(Op(..),Var)
import qualified MultiAnf as Anf(Code(..),Atom(..))

data Result = Result Value Counts

instance Show Result where
  show (Result v counts) =
    unlines [ "value: " ++ show v, "counts:", show counts ]

data Value
  = Number Int
  | Boolean Bool
  | Closure {fvs :: [Value], arity :: Int, body :: Code }

instance Show Value where
  show = \case
    Number n -> show n
    Boolean b -> show b
    Closure{} -> "<closure>"

----------------------------------------------------------------------
-- Closure-Converted Code (CC code)

data Loc = LocArg Int | LocFree Int

data Atom = ALoc Loc | ANum Int

data Code
  = Return Atom
  | Tail Loc [Atom]
  | LetContinue { freeFollow :: [Loc], rhs :: Code, follow :: Code }
  | LetOp Op (Atom,Atom) Code
  | LetClose { freeBody :: [Loc], arity :: Int, body :: Code, code :: Code }
  | Branch Atom Code Code

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
  Return a ->
    ["return: " ++ show a]
  Tail func args ->
    ["tail: " ++ show func ++ " " ++ show args]
  LetContinue{freeFollow,rhs,follow} ->
    indented ("push-k: " ++ show freeFollow ++  " ->") (pretty follow)
    ++ pretty rhs
  LetOp op (a1,a2) code ->
    ["let-op: " ++ show (op,a1,a2)]
    ++ pretty code
  LetClose{freeBody,arity,body,code} ->
    indented ("let-close: " ++ show freeBody ++ " \\" ++ show arity ++ ".") (pretty body)
    ++ pretty code
  Branch a1 c2 c3 ->
    ["if " ++ show a1]
    ++ indented "then" (pretty c2)
    ++ indented "else" (pretty c3)

indented :: String -> [String] -> [String]
indented hang = \case
  [] -> error "indented, no body"
  [oneLine] -> [hang ++ " " ++ oneLine]
  lines -> [hang] ++ ["  " ++ line | line <- lines]


----------------------------------------------------------------------
-- convert Anf to (Closure converted) Code

convert :: Anf.Code -> Code
convert anf = runM (convertAnf anf)

convertAnf :: Anf.Code -> M Code
convertAnf = \case
  Anf.Return a -> Return <$> convertAtom a

  Anf.Tail func args -> do
    func <- Lookup func
    args <- mapM convertAtom args
    return $ Tail func args

  Anf.LetOp x op (a1,a2) code -> do
    a1 <- convertAtom a1
    a2 <- convertAtom a2
    code <- Extend [x] $ convertAnf code
    return $ LetOp op (a1,a2) code

  Anf.LetLam y (xs,body) code -> do
    let fvs = Set.toList $ fvsBinding (xs,body)
    let arity = length xs
    let locations = [ (v,LocFree i) | (v,i) <- zip fvs [0..] ]
    freeBody <- Extend ["_self"] $ mapM Lookup fvs
    body <- Reset locations $ Extend xs $ convertAnf body
    code <- Extend [y] $ convertAnf code
    return $ LetClose {freeBody,arity,body,code}

  Anf.LetFix f (xs,body) code -> do
    let fvs = Set.toList $ fvsBinding (xs,body)
    let arity = length xs
    let locations = [ (v,LocFree i) | (v,i) <- zip fvs [0..] ]
    freeBody <- Extend [f] $ mapM Lookup fvs
    body <- Reset locations $ Extend xs $ convertAnf body
    code <- Extend [f] $ convertAnf code
    return $ LetClose {freeBody,arity,body,code}

  Anf.Branch a1 c2 c3 -> do
    a1 <- convertAtom a1
    c2 <- convertAnf c2
    c3 <- convertAnf c3
    return $ Branch a1 c2 c3

  Anf.LetCode y rhs follow -> do
    let fvs = Set.toList $ fvsBinding ([y],follow)
    let locations = [ (v,LocFree i) | (v,i) <- zip fvs [0..] ]
    freeFollow <- mapM Lookup fvs
    rhs <- convertAnf rhs
    follow <- Reset locations $ Extend [y] $ convertAnf follow
    return $ LetContinue {freeFollow,rhs,follow}

convertAtom :: Anf.Atom -> M Atom
convertAtom = \case
  Anf.ANum n -> return $ ANum n
  Anf.AVar x -> ALoc <$> Lookup x


fvsBinding :: ([Var],Anf.Code) -> Set Var
fvsBinding (vars,code) = fvsCode code \\ Set.fromList vars

fvsCode :: Anf.Code -> Set Var
fvsCode = \case
  Anf.Return a -> fvsAtom a
  Anf.Tail func args -> Set.singleton func <> Set.unions (map fvsAtom args)
  Anf.LetCode x rhs follow -> fvsCode rhs <> fvsBinding ([x],follow)
  Anf.LetOp x _ (a1,a2) code -> fvsAtom a1 <> fvsAtom a2 <> fvsBinding ([x],code)
  Anf.LetLam y (xs,body) code -> fvsBinding (xs,body) <> fvsBinding ([y],code)
  Anf.LetFix f (xs,body) code -> fvsBinding (f:xs,body) <> fvsBinding ([f],code)
  Anf.Branch a1 c2 c3 -> fvsAtom a1 <> fvsCode c2 <> fvsCode c3

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
  Extend :: [Var] -> M a -> M a
  Reset :: [(Var,Loc)] -> M a -> M a

runM :: M a -> a
runM = loop 0 Map.empty where
  loop :: Int -> LocEnv -> M a -> a
  loop d env = \case
    Ret x -> x
    Bind m f -> loop d env (f (loop d env m))
    Reset env m -> loop 0 (Map.fromList env) m
    Lookup x ->
      maybe (error $ "compile-time-lookup:"<>show(x,env)) (rel d) (Map.lookup x env)
    Extend xs m -> do
      let env' = Map.fromList [ (x,LocArg i) | (x,i) <- zip (reverse xs) [d..] ]
      loop (d + length xs) (Map.union env' env) m

rel :: Int -> Loc -> Loc -- relativize a location to a stack depth
rel d = \case
  loc@LocFree{} -> loc
  LocArg n -> LocArg (d-n-1)

type LocEnv = Map Var Loc


----------------------------------------------------------------------
-- machine to execute the closure-converted-code

type Machine {-m-} = (Counts,Control,Frame,Kont)
data Counts  {-i-} = Counts (Map Micro Int)
type Control {-c-} = Code
data Frame   {-f-} = Frame { fvs :: [Value], args :: [Value] }
data Kont    {-k-} = Kdone | Kbind { fvs :: [Value], code :: Code, kont :: Kont }

execute :: Code -> Result
execute = run. install

install :: Code -> Machine
install code = (counts0,code,frame0,Kdone) where frame0 = Frame [] []

run :: Machine -> Result
run (i,code0,f,k) = case code0 of

  Return atom ->
    ret i (atomic f atom) k

  Tail func args ->
    enter i (locate f func) (map (atomic f) args) k

  LetContinue{freeFollow,rhs,follow} ->
    run (tick [DoPushContinuation] i, rhs,f,k')
    where
      k' = Kbind {fvs = map (locate f) freeFollow, code = follow, kont=k}

  LetOp op (a1,a2) code ->
    run (tick [DoPrim op] i, code, f', k)
    where
      f' = push (doOp op (atomic f a1) (atomic f a2)) f

  LetClose {freeBody,arity,body,code} ->
    run (tick [DoMakeClosure] i, code, f', k)
    where
      f' = push clo f
      clo = Closure {fvs = map (locate f') freeBody, arity, body}

  Branch a1 c2 c3 ->
    run (tick [DoBranch] i, branch c2 c3 (atomic f a1), f, k)

ret :: Counts -> Value -> Kont -> Result
ret i v = \case
  Kdone -> Result v i'
  Kbind {fvs,code,kont} -> run (i', code, Frame {fvs, args = [v]}, kont)
  where i' = tick [DoReturn] i

enter :: Counts -> Value -> [Value] -> Kont -> Result
enter i func args k = case func of
  Number{} -> error "cant enter a number"
  Boolean{} -> error "cant enter a boolean"
  clo@Closure{fvs,arity,body}
    | arity == n -> do
        run (tick [DoEnter] i, body, Frame {fvs,args}, k)
    | arity < n -> do
        let (myArgs,overArgs) = splitAt arity args
        let k' = makeOverAppK overArgs k
        run (tick [DoPushOverApp, DoEnter] i, body, Frame {fvs,args = myArgs}, k')
    | otherwise -> do
        ret (tick [DoMakePap] i) (makePap nMissing clo args) k
    where
      nMissing = arity - n
      n = length args

branch :: Code -> Code -> Value -> Code
branch c2 c3 = \case
  Boolean True -> c2
  Boolean False -> c3
  _ -> error "cant branch on a non boolean"

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
  | DoPushOverApp
  | DoPrim Op
  | DoMakeClosure
  | DoMakePap
  | DoBranch
  deriving (Show,Eq,Ord)
