
module BC1(encode,execute) where
-- ByteCode1: encoded ANF
-- Compiler("encode") from ANF
-- (CEK style) Machine to execute ByteCode1

import Control.Monad(ap,liftM)

import Ast
import qualified Anf(Code(..),Atom(..))

data Result = Result [Machine] Value

instance Show Result where
  show (Result _ms v) = unlines (
    --map show ms ++
    ["final value: " ++ show v]
    )

data Value
  = Number Int
  | Clo [Value] DefIndex

instance Show Value where
  show = \case
    Number n -> show n
    Clo vs def -> "Clo" ++ show (vs,def)

----------------------------------------------------------------------
-- encode Anf to ByteCode...

encode :: Anf.Code -> ByteCode
encode anf = runM (encodeAnf anf)

encodeAnf :: Anf.Code -> M ()
encodeAnf = \case
  Anf.Return a -> do Emit RET; encodeAtom a

  Anf.Tail xf a -> do Emit TAIL; encodeVar xf; encodeAtom a

  Anf.LetCode x rhs body -> do
    i <- Define (Extend x $ encodeAnf body)
    Emit PUSH
    Emit (INDEX i)
    encodeAnf rhs

{-  Anf.LetAdd x (a1,a2) body -> do
    Emit ADD
    encodeAtom a1
    encodeAtom a2
    Extend x $ encodeAnf body
-}
  Anf.LetLam x (xf,xc) body -> do
    i <- Define (Extend xf $ encodeAnf xc)
    Emit CLOSE
    Emit (INDEX i)
    Extend x $ encodeAnf body

  Anf.LetFix{} -> undefined
  Anf.Branch{} -> undefined
  Anf.LetOp{} -> undefined


encodeAtom :: Anf.Atom -> M ()
encodeAtom = \case
  Anf.AVar x -> do Emit ENV; encodeVar x
  Anf.ANum n -> do i <- Literal (Number n); Emit VAL; Emit (INDEX i)

encodeVar :: Var -> M ()
encodeVar x = do
  i <- Lookup x
  Emit (INDEX i)

instance Functor M where fmap = liftM
instance Applicative M where pure = return; (<*>) = ap
instance Monad M where return = Ret; (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Emit :: Byte -> M ()
  Lookup :: Var -> M Int
  Extend :: Var -> M a -> M a
  Literal :: Value -> M Int
  Define :: M () -> M Int

runM :: M () -> ByteCode
runM m = res where

  res = final { defs = [defHalt,defMain] ++ encodedDefs }
  defHalt = Code [HALT]
  defMain = Code bs
  (final@ByteCode{defs=encodedDefs},(),bs) = loop ee0 state0 m
  ee0 = []
  state0 = ByteCode { lits = [], defs = [] }

  loop :: EE -> State -> M a -> (State,a,[Byte])
  loop e s@ByteCode{lits} = \case
    Ret x -> (s, x, [])
    Bind m f -> do
      let (s1, a1, bs1) = loop e s m
      let (s2, a2, bs2) = loop e s1 (f a1)
      (s2, a2, bs1 ++ bs2)
    Emit b -> (s, (), [b])
    Lookup x -> (s, look x e, [])
    Extend x m -> loop (x:e) s m
    Literal v -> (s { lits = lits ++ [v] }, length lits, [])
    Define m -> do
      let (s'@ByteCode{defs},(),bs) = loop e s m
      (s' { defs = defs ++ [Code bs] }, 2 + length defs, []) -- for main(1)/halt(0)

type EE {- encoding env -} = [Var]
type Loc = Int
type State = ByteCode

look :: Var -> EE -> Loc
look x ys = case [ i | (y,i) <- zip ys [0..], x==y ] of
  [] -> error $ "encode-time-lookup:" <> show (x,ys)
  i:_ -> i

----------------------------------------------------------------------
-- definition of ByteCode...

data ByteCode = ByteCode { defs :: [Code], lits :: [Value] }

instance Show ByteCode where
  show ByteCode{defs,lits} =
    unlines
    ( ("lits = " ++ show lits)
    : [ show (DefIndex i) ++ " = " ++ show code | (i,code) <- zip [0::Int ..] defs ] )

newtype DefIndex = DefIndex Int

instance Show DefIndex where
  show (DefIndex i) = "#" ++ show i

data Code = Code [Byte]

instance Show Code where
  show (Code bytes) = concat (map show bytes)

data Byte
  = INDEX Int -- index (meaning given by preceeding byte)
  | ADD   -- add, next two bytes locate first arg; following two locate 2nd arg
  | TAIL  -- tail call, next byte is env-index for func, following two locates the arg
  | RET   -- return, next two bytes locate the value
  | CLOSE -- make-closure, next byte is code definition index
  | PUSH  -- push-continuation, next byte is code definition index
  | VAL   -- literal value, next byte indexes the literal value
  | ENV   -- literal value, next byte indexes the env-stack
  | HALT  -- halt machine, result is at top of env-stack

instance Show Byte where
  show = \case
    INDEX i -> show i
    ADD -> "+"
    TAIL -> "t"
    RET -> "r"
    CLOSE -> "c"
    PUSH -> "p"
    VAL -> "v"
    ENV -> "e"
    HALT -> "h"

----------------------------------------------------------------------
type Machine {-m-} = (Control,Env,Kont)
type Control {-c-} = Code
type Env     {-e-} = [Value]
type Kont    {-k-} = [(Env,DefIndex)]

execute :: ByteCode -> Result
execute ByteCode{defs,lits} = run init
  where

    init :: Machine
    init = (lookDef (DefIndex 1), [], [([],DefIndex 0)])

    lookDef :: DefIndex -> Code
    lookDef (DefIndex i) = nth "lookDef" defs i

    run :: Machine -> Result
    run = \case
      m@(Code (HALT:_),(v:_),[]) -> Result [m] v
      m -> let (Result ms v) = run (step m) in Result (m:ms) v

    step :: Machine -> Machine
    step (Code bytes, e, k) = case bytes of

      [] -> error "run out of byte code"
      HALT:_ -> error "cant step a halted machine"
      INDEX{}:_ -> error "unexpected INDEX byte"
      ENV: _ -> error "unexpected ENV byte"
      VAL: _ -> error "unexpected VAL byte"

      ADD:x1:x2:x3:x4:c -> (Code c, add (get x1 x2) (get x3 x4) : e, k)
      ADD:_ -> error "expected four bytes after A"

      [RET,x1,x2] ->
        case k of
          [] -> error "no continuation to return to"
          (e,d):k -> (code, get x1 x2 : e, k) where code = lookDef d
      RET : _ -> error "unexpected non-final RET"

      CLOSE:INDEX(i):c -> (Code c, Clo e (DefIndex i) : e, k)
      CLOSE:_ -> error "expected INDEX byte after CLOSE"

      PUSH:INDEX(i):c -> (Code c, e, (e, DefIndex i) : k)
      PUSH : _ -> error "expected INDEX byte after TAIL"

      [TAIL, (INDEX i), x1, x2] -> (code, get x1 x2 : env, k)
        where
          Clo env def = nth "tail" e i
          code = lookDef def
      TAIL : _ -> error "unexpected non-final TAIL"

      where
        get :: Byte -> Byte -> Value
        get ENV (INDEX i) = nth "ENV" e i
        get VAL (INDEX i) = nth "VAL" lits i
        get _ _ = error "get expected index byte"

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1+n2)
add _ _ = error "can't add non-numbers"

nth :: Show a => String -> [a] -> Int -> a
nth tag xs i = if i >= length xs then error (show (tag,i,xs)) else xs !! i
