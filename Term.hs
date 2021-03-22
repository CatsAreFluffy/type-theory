module Term where

data Level =
  LevelN Int
  | LevelW
  | LevelAfterW
  deriving (Show, Eq, Ord)

data Term =
  Lam Term
  | App Term Term
  | Var Int
  | Zero
  | Succ Term
  | NatRec Term Term Term Term
  | Pi Term Term
  | Bottom
  | Top
  | Nat
  | Sort Level
  | Substed Subst Term
  deriving (Show, Eq)

data Subst =
  IdS
  | CompS Subst Subst
  | WeakenS Int
  | ExtendS Subst Term
  deriving (Show, Eq)

weaken1 :: Subst
weaken1 = WeakenS 1

sortN :: Int -> Term
sortN = Sort . LevelN

star :: Term
star = sortN 0

box :: Term
box = sortN 1

fun :: Term -> Term -> Term
fun a b = Pi a $ Substed weaken1 b

bindingSubst :: Term -> Subst
bindingSubst = ExtendS IdS

delaySubst :: Subst -> Subst
delaySubst s = ExtendS (CompS weaken1 s) (Var 0)

bubbleSubsts :: Term -> Term
bubbleSubsts (Lam x) = Lam (bubbleSubsts x)
bubbleSubsts (App x y) = App (bubbleSubsts x) (bubbleSubsts y)
bubbleSubsts (Var n) = Var n
bubbleSubsts (Zero) = Zero
bubbleSubsts (Succ x) = Succ (bubbleSubsts x)
bubbleSubsts (NatRec t x y n) =
  NatRec (bubbleSubsts t) (bubbleSubsts x) (bubbleSubsts y) (bubbleSubsts n)
bubbleSubsts (Pi x y) = Pi (bubbleSubsts x) (bubbleSubsts y)
bubbleSubsts (Top) = Top
bubbleSubsts (Bottom) = Bottom
bubbleSubsts (Nat) = Nat
bubbleSubsts (Sort k) = Sort k
bubbleSubsts (Substed s x) = substTerm s (bubbleSubsts x)

substTerm :: Subst -> Term -> Term
substTerm s (Lam x) = Lam $ substTerm (delaySubst s) x
substTerm s (App x y) = App (substTerm s x) (substTerm s y)
substTerm s (Zero) = Zero
substTerm s (Succ x) = Succ (substTerm s x)
substTerm s (NatRec t x y n) =
  NatRec (substTerm s t) (substTerm s x) (substTerm s' y) (substTerm s n)
  where s' = delaySubst $ delaySubst s
substTerm s (Pi x y) = Pi (substTerm s x) (substTerm (delaySubst s) y)
substTerm s (Top) = Top
substTerm s (Bottom) = Bottom
substTerm s (Nat) = Nat
substTerm s (Sort k) = Sort k
substTerm s (Substed s' x) = Substed (CompS s s') x
substTerm (CompS s s') v@(Var _) = substTerm s $ substTerm s' v
substTerm (IdS) v@(Var _) = v
substTerm (WeakenS n) (Var n') = Var $ n' + n
substTerm (ExtendS s x) (Var 0) = x
substTerm (ExtendS s x) (Var n) = substTerm s (Var $ n - 1)
