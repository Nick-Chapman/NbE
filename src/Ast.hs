
module Ast(Var,Exp(..),Op(..),ppOp) where
-- AST for user expressions

type Var = String

data Exp
  = Num { unNum :: Int }
  | Prim Op
  | SatPrim Exp Op Exp
  | Var Var
  | Lam Var Exp
  | App Exp Exp
  | Let Var Exp Exp
  | Fix Var Exp
  | Ite Exp Exp Exp

data Op = Add | Sub | Mul | Leq
  deriving (Eq,Ord,Show)

instance Show Exp where
  show = \case
    Num n -> show n
    Prim op -> "(" ++ ppOp op ++ ")"
    SatPrim e1 op e2 -> "(" ++ show e1 ++ ppOp op ++ show e2 ++ ")"
    Var s -> s
    Lam x body -> "(\\" ++ x ++ "." ++ show body ++ ")"
    App e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    Let x e1 e2 -> "(let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
    Fix x e -> "(fix " ++ x ++ " in " ++ show e ++ ")"
    Ite e1 e2 e3 -> "(if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3 ++ ")"

ppOp :: Op -> String
ppOp = \case
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Leq -> "<="

