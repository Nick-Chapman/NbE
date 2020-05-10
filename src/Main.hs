
module Main(main) where
-- Run evaluators on a selected example

import System.Environment (getArgs)
import qualified Data.Map.Strict as Map

import Ast
import Examples(examples)
import qualified Evaluate as Eval(evaluate)
import qualified Cek(evaluate)
import qualified Anf(flatten)
import qualified Cek_Anf(execute)
import qualified BC1(encode,execute)
import qualified ClosureConvert as CC(convert,execute)

import Normalize(normalize)

main :: IO ()
main = do
  let defaultProg = "thrice-thrice-dec"
  args <- getArgs
  let name = case args of [] -> defaultProg; [x] -> x; _ -> error (show args)
  let prog = maybe (error $ "unknown program: "++name) id (Map.lookup name examples)
  stages "original" prog
  stages "optimized" (normalize prog)

stages :: String -> Exp -> IO ()
stages tag exp = do
  print "--------------------------------------------------"
  print tag
  print "--------------------------------------------------"
  print exp

  print "Eval"; print (Eval.evaluate exp)
  print "Cek"; print (Cek.evaluate exp)

  print "compile: AST -> ANF"
  let anf = Anf.flatten exp
  print anf

  print "Execute(ANF)..."
  print (Cek_Anf.execute anf)

  print "encode: ANF -> ByteCode1"
  let bc1 = BC1.encode anf
  print bc1

  print "Execute(ByteCode1)..."
  print (BC1.execute bc1)

  print "ClosureConvert: ANF -> CC"
  let cc = CC.convert anf
  print cc
  print "Execute(CC)..."
  print (CC.execute cc)
