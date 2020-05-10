
module Meh.Eval_ExplicitSubst(evaluate) where

import Ast

evaluate :: Exp -> Exp
evaluate = loop
  where
    loop prog = do
      case findRedex prog of
        Just (redex, contextF) -> do
          let reduced = reduceRedex redex
          let prog' = contextF reduced
          loop prog'
        Nothing ->
          prog


reduceRedex :: Exp -> Exp
reduceRedex = \case
  App (Lam x body) arg -> subst (x,arg) body
  SaturatedAdd (Num a) (Num b) -> Num (a+b)
  App (App AddOp (Num a)) (Num b) -> Num (a+b)
  Let x rhs body -> subst (x,rhs) body
  _ -> error "not a redex"


findRedex :: Exp -> Maybe (Exp, Exp -> Exp)
findRedex = \case
  red@(App (Lam _ _) _)                 -> Just (red, \x -> x) -- beta redex (normal order)
  red@(SaturatedAdd (Num _) (Num _))    -> Just (red, \x -> x) -- delta redex
  red@(App (App AddOp (Num _)) (Num _)) -> Just (red, \x -> x) -- delta redex
  red@(Let _ _ _)                       -> Just (red, \x -> x) -- beta(ish) redex
  SaturatedAdd e1 e2 -> do
    case findRedex e1 of
      Just (red, contextF) -> Just (red, \red -> SaturatedAdd (contextF red) e2)
      Nothing ->
        case findRedex e2 of
          Just (red, contextF) -> Just (red, \red -> SaturatedAdd e1 (contextF red))
          Nothing ->
            Nothing
  App e1 e2 -> do
    case findRedex e1 of
      Just (red, contextF) -> Just (red, \red -> App (contextF red) e2)
      Nothing ->
        case findRedex e2 of
          Just (red, contextF) -> Just (red, \red -> App e1 (contextF red))
          Nothing ->
            Nothing
  -- note: we dont look for a redex inside a lambda
  _ -> Nothing


subst :: (Var,Exp) -> Exp -> Exp
subst p@(x,replacement) = \case
  e@(Num _) -> e
  e@(Var y) -> if x==y then replacement else e
  e@AddOp-> e
  SaturatedAdd e1 e2 -> SaturatedAdd (subst p e1) (subst p e2)
  App e1 e2 -> App (subst p e1) (subst p e2)
  e@(Lam y body)
    | x == y -> e
    | y `notElem` fvs replacement -> Lam y (subst p body)
    | otherwise -> undefined -- need to alpha rename: \y.body -> \z.body[y:=z]
  Let y rhs body
    | x == y -> Let y (subst p rhs) body
    | y `notElem` fvs replacement -> Let y (subst p rhs) (subst p body)
    | otherwise -> undefined -- need to alpha rename


fvs :: Exp -> [Var]
fvs = \case
  Var v -> [v]
  AddOp-> []
  SaturatedAdd e1 e2 -> fvs e1 ++ fvs e2
  App e1 e2 -> fvs e1 ++ fvs e2
  Lam x e -> fvs e // x
  Num{} -> []
  Let x rhs body -> fvs rhs ++ (fvs body // x)
  where
    ys // x = [ y | y <- ys, x /= y ]
