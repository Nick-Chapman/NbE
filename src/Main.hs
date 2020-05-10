
module Main(main) where

import System.Environment (getArgs)
import qualified Data.Map.Strict as Map

import Ast
import Examples(examples)
--import Eval_ExplicitSubst(evaluate)
--import Eval_Environment(evaluate)
--import Eval(evaluate)
--import Eval_Instrumented(evaluate)
--import qualified Cek(evaluate)
import Cek4(evaluate)
import qualified Cek5(compile,execute)
--import qualified BC1(compile,execute)
import qualified ClosureConvert as CC(compile,execute)

--import Norm_Final(normalize)

main :: IO ()
main = do
  let defaultProg = "thrice-thrice-dec"
  args <- getArgs
  let name = case args of [] -> defaultProg; [x] -> x; _ -> error (show args)
  let prog = maybe (error $ "unknown program: "++name) id (Map.lookup name examples)
  demo "original" prog
  --demo "optimized" (normalize prog)

demo :: String -> Exp -> IO ()
demo tag exp = do
  print tag
  print exp

  print "Eval"; print (evaluate exp)

  print "CEK5"
  let code = Cek5.compile exp
  print code
  print (Cek5.execute code)

  --print "BC1"
  --let code = BC1.compile exp
  --print code; print (BC1.execute code)

  print "ClosureConverted"
  let code = CC.compile exp
  print code; print (CC.execute code)
