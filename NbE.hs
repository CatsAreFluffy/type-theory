module NbE where

import Term

data Value =
  VLam Closure
  | VZero
  | VSucc Value
  | VPi Value Closure
  | VBottom
  | VTop
  | VNat
  | VSort Level
  | Reflect Value Neutral
  deriving Show

vStar :: Value
vStar = VSort $ LevelN 0

vBox :: Value
vBox = VSort $ LevelN 1

data Neutral =
  NVar Int
  | NApp Neutral Normal
  | NNatRec Neutral Closure Value Closure
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
eval (Zero) e = VZero
eval (Succ x) e = VSucc $ eval x e
eval (NatRec t x y n) e = vNatRec (Closure t e) (eval x e) (Closure y e) (eval n e)
eval (Pi x y) e = VPi (eval x e) (Closure y e)
eval (Bottom) e = VBottom
eval (Top) e = VTop
eval (Nat) e = VNat
eval (Sort k) _ = VSort k
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

vNatRec :: Closure -> Value -> Closure -> Value -> Value
vNatRec t x y (VZero) = x
vNatRec t x y (VSucc z) = inst y [vNatRec t x y z, z]
vNatRec t x y z@(Reflect VNat e) = Reflect (inst t [z]) $ NNatRec e t x y

quoteTypedValue :: Value -> Value -> Int -> Term
quoteTypedValue (VPi a f) x n = Lam $
  quoteTypedValue (inst f [fresh]) (vApp x fresh) (n + 1)
  where fresh = Reflect a (NVar n)
quoteTypedValue (VBottom) (Reflect _ e) n = quoteNeutral e n
-- Top is proof-irrelevant
quoteTypedValue (VTop) _ n = star
quoteTypedValue (VNat) (VZero) n = Zero
quoteTypedValue (VNat) (VSucc x) n = Succ $ quoteTypedValue VNat x n
quoteTypedValue (VNat) (Reflect _ e) n = quoteNeutral e n
quoteTypedValue (VSort _) t n = quoteType t n
quoteTypedValue (Reflect _ _) (Reflect _ e) n = quoteNeutral e n

quoteNormal :: Normal -> Int -> Term
quoteNormal (Normal t x) = quoteTypedValue t x

quoteNeutral :: Neutral -> Int -> Term
quoteNeutral (NVar k) n = Var $ n - (k + 1)
quoteNeutral (NApp x y) n = App (quoteNeutral x n) (quoteNormal y n)
quoteNeutral (NNatRec z t x y) n = NatRec bt bx by (quoteNeutral z n)
  where
    t' = inst t [Reflect VNat $ NVar n]
    bt = quoteType t' (n + 1)
    tx = inst t [VZero]
    bx = quoteTypedValue tx x n
    ty = inst t [VSucc $ Reflect VNat $ NVar n]
    y' = inst y [Reflect t' $ NVar $ n + 1, Reflect VNat $ NVar n]
    by = quoteTypedValue ty y' (n + 2)

quoteType :: Value -> Int -> Term
quoteType (VPi a f) n = Pi (quoteType a n) (quoteType (inst f [fresh]) (n + 1))
  where fresh = Reflect a (NVar n)
quoteType (VBottom) n = Bottom
quoteType (VTop) n = Top
quoteType (VNat) n = Nat
quoteType (VSort k) n = Sort k
quoteType (Reflect _ v) n = quoteNeutral v n

evalContext :: [Term] -> Env
evalContext [] = []
evalContext (x:xs) = (Reflect (eval x vs) (NVar $ length xs)) : vs
  where vs = evalContext xs

normalizeTypeValue :: Env -> Value -> Term
normalizeTypeValue ctx t = quoteType t (length ctx)

normalizeType :: [Term] -> Term -> Term
normalizeType ctx t = normalizeTypeValue sctx (eval t sctx)
  where sctx = evalContext ctx

normalizeValue :: Env -> Value -> Value -> Term
normalizeValue ctx t x = quoteTypedValue t x (length ctx)

normalize :: [Term] -> Term -> Term -> Term
normalize ctx t x = normalizeValue sctx (eval t sctx) (eval x sctx)
  where sctx = evalContext ctx

subtype :: Value -> Value -> Int -> Bool
subtype VBottom _ n = True
subtype _ VTop n = True
subtype (VSort k) (VSort l) n | l < LevelAfterW = k <= l
subtype (VPi a b) (VPi c d) n = subtype c a n &&
  subtype (inst b [fresh]) (inst d [fresh]) (n + 1)
  where fresh = Reflect a (NVar n)
subtype x y n = quoteType x n == quoteType y n
