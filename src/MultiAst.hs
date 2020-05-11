
module MultiAst(trans,Var,Op(..),Exp(..)) where
-- Version of expression AST with multi Apps and multi Lams
-- And compiler("trans") from plain AST

import Data.List(intercalate)
import Ast(Var,Op(..),ppOp)
import qualified Ast

data Exp
  = Num { unNum :: Int }
  | Prim Op
  | SatPrim Exp Op Exp
  | Var Var
  | Lam [Var] Exp
  | App Exp [Exp]
  | Let Var Exp Exp
  | Fix Var Exp
  | Ite Exp Exp Exp

instance Show Exp where
  show = \case
    Num n -> show n
    Prim op -> "(" ++ ppOp op ++ ")"
    SatPrim e1 op e2 -> "(" ++ show e1 ++ ppOp op ++ show e2 ++ ")"
    Var s -> s
    Lam xs body -> "(\\[" ++ comma xs ++ "]." ++ show body ++ ")"
    App func args -> "(" ++ show func ++ "[" ++ comma (map show args) ++ "])"
    Let x e1 e2 -> "(let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
    Fix x e -> "(fix " ++ x ++ " in " ++ show e ++ ")"
    Ite e1 e2 e3 -> "(if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3 ++ ")"

comma :: [String] -> String
comma = intercalate ","

trans :: Ast.Exp -> Exp
trans = \case
    Ast.Num n -> Num n
    Ast.Prim op -> Prim op
    Ast.SatPrim e1 op e2 -> SatPrim (trans e1) op (trans e2)
    Ast.Var s -> Var s
    Ast.Lam x body -> transLam [x] body
    Ast.App func arg -> transApp [arg] func
    Ast.Let x e1 e2 -> Let x (trans e1) (trans e2)
    Ast.Fix x e -> Fix x (trans e)
    Ast.Ite e1 e2 e3 -> Ite (trans e1) (trans e2) (trans e3)

transLam :: [Var] -> Ast.Exp -> Exp
transLam xs = \case
  Ast.Lam x body -> transLam (x:xs) body
  body -> Lam (reverse xs) (trans body)

transApp :: [Ast.Exp] -> Ast.Exp -> Exp
transApp args = \case
  Ast.App func arg -> transApp (arg : args) func
  func -> App (trans func) (map trans args)
