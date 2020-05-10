
module Main(main) where
-- Run evaluators on a selected example

--import Control.Monad(when)
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map

import Examples(examples)
import qualified Ast(Exp)
import qualified Evaluate as Eval(evaluate)
import qualified Cek(evaluate)
import qualified Anf(flatten)
import qualified Cek_Anf(execute)
--import qualified BC1(encode,execute)
import qualified ClosureConvert as CC(convert,execute)

import qualified MultiAst(trans)
import qualified MultiAnf(flatten)

--import Normalize(normalize)

main :: IO ()
main = do
  let defaultProg = "thrice-thrice"
  args <- getArgs
  let name = case args of [] -> defaultProg; [x] -> x; _ -> error (show args)
  let prog = maybe (error $ "unknown program: "++name) id (Map.lookup name examples)
  stages "original" prog
  --stages "optimized" (normalize prog)

stages :: String -> Ast.Exp -> IO ()
stages tag exp = do
  print "--------------------------------------------------"
  print tag
  print "--------------------------------------------------"
  print exp

  print "trans: Ast.Exp -> MultiAst.Exp"
  let mexp = MultiAst.trans exp
  print mexp

  print "Eval"; print (Eval.evaluate exp)
  print "Cek"; print (Cek.evaluate exp)

  print "flatten: Ast.Exp -> Anf.Code"
  let anf = Anf.flatten exp
  print anf

  print "flatten: MultiAst.Exp -> MultiAnf.Code"
  let manf = MultiAnf.flatten mexp
  --print manf

  --print "encode: Anf.Code -> BC1.ByteCode"
  --let bc1 = BC1.encode anf
  --print bc1

  --print "execute(BC1.ByteCode)..."
  --print (BC1.execute bc1)

  print "closure-convert: Anf.Code -> CC.Code"
  let cc = CC.convert manf
  print cc

  print "execute(Anf.Code)..."
  print (Cek_Anf.execute anf)

  print "execute(CC.Code)..."
  print (CC.execute cc)
