module NbE where

import Term

data Value =
  VLam Closure
  | VPi Value Closure
  | VSort Int
  | Reflect Value Neutral
  deriving Show

data Neutral =
  NVar Int
  | NApp Neutral Normal
  deriving Show

type Env = [Value]

data Closure = Closure {cBody :: Term, cEnv :: Env}
  deriving Show

data Normal = Normal {nType :: Value, nVal :: Value}
  deriving Show

eval :: Term -> Env -> Value
eval (Lam x) e = VLam $ Closure x e
eval (App x y) e = vApp (eval x e) (eval y e)
eval (Var n) e = e !! n
eval (Pi x y) e = VPi (eval x e) (Closure y e)
eval (Sort n) _ = VSort n
eval (Substed s x) e = eval x (substEnv s e)

inst :: Closure -> [Value] -> Value
inst (Closure v e) vs = eval v (vs ++ e)

substEnv :: Subst -> Env -> Env
substEnv IdS e = e
substEnv (CompS s s') e = substEnv s $ substEnv s' e
substEnv (WeakenS n) e = drop n e
substEnv (ExtendS s x) e = (eval x e) : (substEnv s e)

vApp :: Value -> Value -> Value
vApp (VLam c) x = inst c [x]
vApp (Reflect (VPi a f) e) x = Reflect (inst f [x]) (NApp e $ Normal a x)

quoteTypedValue :: Value -> Value -> Int -> Term
quoteTypedValue (VPi a f) x n = Lam $
  quoteTypedValue (inst f [fresh]) (vApp x fresh) (n + 1)
  where fresh = Reflect a (NVar n)
quoteTypedValue (VSort k) t n = quoteType t n
quoteTypedValue (Reflect _ _) (Reflect _ e) n = quoteNeutral e n

quoteNormal :: Normal -> Int -> Term
quoteNormal (Normal t x) = quoteTypedValue t x

quoteNeutral :: Neutral -> Int -> Term
quoteNeutral (NVar k) n = Var $ n - (k + 1)
quoteNeutral (NApp x y) n = App (quoteNeutral x n) (quoteNormal y n)

quoteType :: Value -> Int -> Term
quoteType (VPi a f) n = Pi (quoteType a n) (quoteType (inst f [fresh]) (n + 1))
  where fresh = Reflect a (NVar n)
quoteType (VSort k) n = Sort k
quoteType (Reflect _ v) n = quoteNeutral v n

evalContext :: [Term] -> Env
evalContext [] = []
evalContext (x:xs) = (Reflect (eval x vs) (NVar $ length xs)) : vs
  where vs = evalContext xs

normalizeType :: [Term] -> Term -> Term
normalizeType ctx t = quoteType (eval t $ evalContext ctx) (length ctx)

normalize :: [Term] -> Term -> Term -> Term
normalize ctx t x = quoteTypedValue (eval t sctx) (eval x sctx) (length ctx)
  where sctx = evalContext ctx
