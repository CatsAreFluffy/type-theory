module Term where

data Term =
  Lam Term
--  | TypedLam Term Term
  | App Term Term
  | Var Int
  | Pi Term Term
  | Sort Int
  | Substed Subst Term
  deriving (Show, Eq)

instance Ord Term where
  Sort n <= Sort m = n <= m
  x <= y = x == y

data Subst =
  IdS
  | CompS Subst Subst
  | WeakenS Int
  | ExtendS Subst Term
  deriving (Show, Eq)

weaken1 :: Subst
weaken1 = WeakenS 1

star :: Term
star = Sort 0

box :: Term
box = Sort 1

fun :: Term -> Term -> Term
fun a b = Pi a $ Substed weaken1 b

delaySubst :: Subst -> Subst
delaySubst s = ExtendS (CompS s weaken1) (Var 0)

bubbleSubsts :: Term -> Term
bubbleSubsts (Lam x) = Lam (bubbleSubsts x)
-- bubbleSubsts (TypedLam x y) = TypedLam (bubbleSubsts x) (bubbleSubsts y)
bubbleSubsts (App x y) = App (bubbleSubsts x) (bubbleSubsts y)
bubbleSubsts (Var n) = Var n
bubbleSubsts (Pi x y) = Pi (bubbleSubsts x) (bubbleSubsts y)
bubbleSubsts (Sort n) = Sort n
bubbleSubsts (Substed s x) = substTerm s (bubbleSubsts x)

substTerm :: Subst -> Term -> Term
substTerm s (Lam x) = Lam $ substTerm (delaySubst s) x
-- substTerm s (TypedLam x y) =
--     TypedLam (substTerm s x) $ substTerm (delaySubst s) y
substTerm s (App x y) = App (substTerm s x) (substTerm s y)
substTerm s (Pi x y) = Pi (substTerm s x) (substTerm (delaySubst s) y)
substTerm s (Sort n) = Sort n
substTerm s (Substed s' x) = Substed (CompS s s') x
substTerm (CompS s s') v@(Var _) = substTerm s $ substTerm s' v
substTerm (IdS) v@(Var _) = v
substTerm (WeakenS n) (Var n') = Var $ n' + n
substTerm (ExtendS s x) (Var 0) = x
substTerm (ExtendS s x) (Var n) = substTerm s (Var $ n - 1)