module NbE where

import Term

data Value =
  VLam Closure
  | VAbort Value
  | VZero
  | VSucc Value
  | VAddProof Value Value Value
  | VPi Value Closure
  | VRelevantBottom
  | VBottom
  | VTop
  | VNat
  | VRefine Value Closure
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
  | NUseProof' Neutral Value Value Closure Closure
  deriving Show

type Env = [Value]

data Closure = Closure {cBody :: Term, cEnv :: Env}
--  deriving Show

instance Show Closure where
  show = show . cBody

data Normal = Normal {nType :: Value, nVal :: Value}
  deriving Show

maxPiSubtype :: Env -> Value -> Either String (Value, Closure)
maxPiSubtype c (VAddProof t _ _) = maxPiSubtype c t
maxPiSubtype c (VPi a f) = return (a, f)
maxPiSubtype c (VTop) = return (VBottom, Closure Top c)
maxPiSubtype _ t = Left $ show t ++ " isn't a supertype of a pi"

minPiSupertype :: Env -> Value -> Either String (Value, Closure)
-- minPiSupertype c t | trace ("minPi\n" ++ show c ++ '\n':show t) False = undefined
minPiSupertype c (VAddProof t _ _) = minPiSupertype c t
minPiSupertype c (VPi a f) = return (a, f)
minPiSupertype c (VBottom) = return (VTop, Closure Bottom c)
minPiSupertype c (VRefine t _) = minPiSupertype c t
minPiSupertype _ t = Left $ show t ++ " isn't a subtype of a pi"

eval :: Term -> Env -> Value
eval (Lam x) e = VLam $ Closure x e
eval (App x y) e = vApp (eval x e) (eval y e)
eval (Var n) e = e !! n
eval (Abort (Irrel x)) e = VAbort $ vElimAbort $ eval x e
eval (Zero) _ = VZero
eval (Succ x) e = VSucc $ eval x e
eval (NatRec t x y n) e = vNatRec (Closure t e) (eval x e) (Closure y e) (eval n e)
eval (AddProof x t (Irrel p)) e = VAddProof (eval x e) (eval t e) (eval p e)
eval (UseProof tx tp x ty y) e =
  vUseProof (eval tx e) (eval tp e) (eval x e) (Closure ty e) (Closure y e) (length e)
eval (Pi x y) e = VPi (eval x e) (Closure y e)
eval (Bottom) _ = VBottom
eval (Top) _ = VTop
eval (Nat) _ = VNat
eval (Refine x p) e = vRefine (eval x e) (Closure p e)
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
vApp (VAbort f) x = vApp f x
vApp (VAddProof f _ _) x = vApp f x
-- this is going to bite me in the ass someday
vApp (Reflect t e) x | Right (a, f) <- minPiSupertype [] t
  = Reflect (inst f [x]) (NApp e $ Normal a x)

vElimAbort :: Value -> Value
vElimAbort (VAbort x) = x
vElimAbort (VAddProof x _ _) = vElimAbort x
vElimAbort (Reflect _ e) = Reflect VRelevantBottom e

vNatRec :: Closure -> Value -> Closure -> Value -> Value
vNatRec t x y (VAbort z) = vNatRec t x y z
vNatRec t x y (VZero) = x
vNatRec t x y (VSucc z) = inst y [vNatRec t x y z, z]
vNatRec t x y (VAddProof z _ _) = vNatRec t x y z
vNatRec t x y z@(Reflect _ e) = Reflect (inst t [z]) $ NNatRec e t x y

-- useproof{*;*{Top:*}:?[p.*];t.?[p.*];x p.x}
vUseProof :: Value -> Value -> Value -> Closure -> Closure -> Int -> Value
vUseProof tx tp (VAbort x) ty y n = vUseProof tx tp x ty y n
vUseProof tx tp v@(VAddProof x tp' p') ty y n
  | subtype tp' tp (n + 1) = inst y [p', v]
  | otherwise = vUseProof tx tp x ty y n
vUseProof tx tp v@(Reflect _ e) ty y _ =
  Reflect t' $ NUseProof' e tx tp ty y
  where t' = inst ty [v]
vUseProof t x p y n q = error $ show p ++ show x ++ show t ++ show y ++ show n

-- I really don't like this
vRefine :: Value -> Closure -> Value
vRefine VBottom _ = VBottom
vRefine x p = VRefine x p

quoteAbort :: Value -> Int -> Term
quoteAbort x n = quoteTypedValue VBottom x n

quoteTypedValue :: Value -> Value -> Int -> Term
quoteTypedValue (VAbort t) x n = quoteTypedValue t x n
quoteTypedValue (VAddProof t _ _) x n = quoteTypedValue t x n
quoteTypedValue (VPi a f) x n = Lam $
  quoteTypedValue (inst f [fresh]) (vApp x fresh) (n + 1)
  where fresh = Reflect a (NVar n)
quoteTypedValue (VBottom) x n =
  Abort $ Irrel $ quoteTypedValue VRelevantBottom (vElimAbort x) n
-- quoteAbort doesn't work here
quoteTypedValue (VRelevantBottom) (VAbort x) n = quoteAbort x n
  --quoteTypedValue VRelevantBottom x n
quoteTypedValue (VRelevantBottom) (VAddProof x _ _) n =
  quoteTypedValue VRelevantBottom x n
-- quoteTypedValue (VRelevantBottom) v@(Reflect _ _) n =
--   quoteReflectInType VRelevantBottom v n
quoteTypedValue (VTop) _ n = star
quoteTypedValue (VNat) (VAbort x) n = quoteTypedValue VBottom x n
quoteTypedValue (VNat) (VZero) n = Zero
quoteTypedValue (VNat) (VSucc x) n = Succ $ quoteTypedValue VNat x n
quoteTypedValue (VNat) (VAddProof x _ _) n = quoteTypedValue VNat x n
-- quoteTypedValue (VNat) v@(Reflect _ _) n = quoteReflectInType VNat v n
quoteTypedValue (VRefine _ _) (VAbort x) n = quoteAbort x n
quoteTypedValue tt@(VRefine t r) (VAddProof x tp p) n
  | subtype tp (inst r [x]) n =
    AddProof (quoteTypedValue t x n) (quoteType tp n)
    $ Irrel $ quoteTypedValue (inst r [x]) p (n + 1)
  | otherwise = quoteTypedValue tt x n
quoteTypedValue (VSort _) t n = quoteType t n
quoteTypedValue (Reflect _ _) (VAbort x) n = quoteAbort x n
quoteTypedValue t@(Reflect _ _) (VAddProof x _ _) n = quoteTypedValue t x n
quoteTypedValue t v@(Reflect _ _) n = quoteReflectInType t v n

quoteNormal :: Normal -> Int -> Term
quoteNormal (Normal t x) = quoteTypedValue t x

-- I don't like this
quoteReflectInType :: Value -> Value -> Int -> Term
quoteReflectInType t v@(Reflect t' e) n
  | valueTypeEq t t' n = quoteNeutral e n
  | otherwise = quoteTypedValue t' v n

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
    --input inputtype inputprop outtype out
quoteNeutral (NUseProof' e tx tp ty y) n =
  UseProof (quoteType tx n) (quoteType tp n) (quoteNeutral e n)
  (quoteType ty' (n + 1)) (quoteTypedValue ty' y' (n + 2))
  where
    --p' = inst p [Reflect x $ NVar n]
    ty' = inst ty [Reflect tx $ NVar n]
    y' = inst y [Reflect tp $ NVar $ n + 1, Reflect tx $ NVar n]

quoteType :: Value -> Int -> Term
quoteType (VAbort x) n = quoteAbort x n
quoteType (VAddProof t _ _) n = quoteType t n
quoteType (VPi a f) n = Pi (quoteType a n) (quoteType (inst f [fresh]) (n + 1))
  where fresh = Reflect a (NVar n)
quoteType (VBottom) n = Bottom
-- might not be a good idea
quoteType (VRelevantBottom) n = Bottom
quoteType (VTop) n = Top
quoteType (VNat) n = Nat
quoteType (VRefine t p) n = Refine (quoteType t n) (quoteType (inst p [fresh]) (n + 1))
  where fresh = Reflect t (NVar n)
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
subtype (VAddProof a _ _) b n = subtype a b n
subtype a (VAddProof b _ _) n = subtype a b n
subtype VBottom _ n = True
subtype _ VTop n = True
subtype (VRefine a p) (VRefine b q) n
  | subtype a b n, 
    let x = Reflect a (NVar n) in
      subtype (inst p [x]) (inst q [x]) (n + 1)
    = True
subtype (VRefine a x) b n = subtype a b n
subtype (VSort k) (VSort l) n | l < LevelAfterW = k <= l
subtype (VPi a b) (VPi c d) n = subtype c a n &&
  subtype (inst b [fresh]) (inst d [fresh]) (n + 1)
  where fresh = Reflect a (NVar n)
subtype x y n = valueTypeEq x y n

valueTypeEq :: Value -> Value -> Int -> Bool
valueTypeEq x y n = quoteType x n == quoteType y n

valueEq :: Value -> Value -> Value -> Int -> Bool
valueEq t x y n = quoteTypedValue t x n == quoteTypedValue t y n
