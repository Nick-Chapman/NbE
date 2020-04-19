
module Ast where

type Var = String

data Exp
  = Num Int
  | Add Exp Exp
  | Let Var Exp Exp
  | Var Var
  | Lam Var Exp
  | App Exp Exp

  deriving (Show)
