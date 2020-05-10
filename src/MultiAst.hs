
module MultiAst(trans,Var,Exp(..)) where
-- Version of expression AST with multi Apps and multi Lams
-- And compiler("trans") from plain AST

import Data.List(intercalate)
import qualified Ast

type Var = Ast.Var

data Exp
  = Num { unNum :: Int }
  | AddOp
  | SaturatedAdd Exp Exp
  | Var Var
  | Lam [Var] Exp
  | App Exp [Exp]
  | Let Var Exp Exp

instance Show Exp where
  show = \case
    Num n -> show n
    AddOp -> "(+)"
    SaturatedAdd e1 e2 -> "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    Var s -> s
    Lam xs body -> "(\\[" ++ comma xs ++ "]." ++ show body ++ ")"
    App func args -> "(" ++ show func ++ "[" ++ comma (map show args) ++ "])"
    Let x e1 e2 -> "(let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"

comma :: [String] -> String
comma = intercalate ","

trans :: Ast.Exp -> Exp
trans = \case
    Ast.Num n -> Num n
    Ast.AddOp -> AddOp
    Ast.SaturatedAdd e1 e2 -> SaturatedAdd (trans e1) (trans e2)
    Ast.Var s -> Var s
    Ast.Lam x body -> transLam [x] body
    Ast.App func arg -> transApp [arg] func
    Ast.Let x e1 e2 -> Let x (trans e1) (trans e2)

transLam :: [Var] -> Ast.Exp -> Exp
transLam xs = \case
  Ast.Lam x body -> transLam (x:xs) body
  body -> Lam (reverse xs) (trans body)

transApp :: [Ast.Exp] -> Ast.Exp -> Exp
transApp args = \case
  Ast.App func arg -> transApp (arg : args) func
  func -> App (trans func) (map trans args)
