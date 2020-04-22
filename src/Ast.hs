
module Ast where

type Var = String

data Exp
  = Num { unNum :: Int }
  | Add Exp Exp
  | Var Var
  | Lam Var Exp
  | App Exp Exp
  | Let Var Exp Exp

instance Show Exp where
  show = \case
    Num n -> show n
    Add e1 e2 -> "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    Var s -> s
    Lam s body -> "(\\" ++ s ++ "." ++ show body ++ ")"
    App e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    Let x e1 e2 -> "(let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
