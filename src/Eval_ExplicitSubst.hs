
module Eval_ExplicitSubst(evaluate) where

import Ast

evaluate :: Exp -> Int
evaluate = loop
  where
    loop prog = do
      case findRedex prog of
        Just (redex, contextF) -> do
          let reduced = reduceRedex redex
          let prog' = contextF reduced
          loop prog'
        Nothing ->
          unNum prog


reduceRedex :: Exp -> Exp
reduceRedex = \case
  App (Lam x body) arg -> subst (x,arg) body
  Add (Num a) (Num b) -> Num (a+b)
  _ -> error "not a redex"


findRedex :: Exp -> Maybe (Exp, Exp -> Exp)
findRedex = \case
  red@(App (Lam _ _) _)     -> Just (red, \x -> x)      -- beta redex (normal order)
  red@(Add (Num _) (Num _)) -> Just (red, \x -> x)      -- delta redex
  Add e1 e2 -> do
    case findRedex e1 of
      Just (red, contextF) -> Just (red, \red -> Add (contextF red) e2)
      Nothing ->
        case findRedex e2 of
          Just (red, contextF) -> Just (red, \red -> Add e1 (contextF red))
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
  Add e1 e2 -> Add (subst p e1) (subst p e2)
  App e1 e2 -> App (subst p e1) (subst p e2)
  e@(Lam y body)
    | x == y -> e
    | y `notElem` fvs replacement -> Lam y (subst p body)
    | otherwise -> undefined -- need to alpha rename: \y.body -> \z.body[y:=z]
  Let{} -> undefined


fvs :: Exp -> [Var]
fvs = \case
  Var v -> [v]
  Add e1 e2 -> fvs e1 ++ fvs e2
  App e1 e2 -> fvs e1 ++ fvs e2
  Lam x e -> [ y | y <- fvs e, x /= y ]
  Num{} -> []
  Let{} -> undefined
