module TypedTerm where

import Term

data CheckedTerm =
  Synthed SynthedTerm
  | TLam CheckedTerm
  | TAddProof CheckedTerm CheckedTerm CheckedTerm
  | TLetC SynthedTerm CheckedTerm
  deriving Show

data SynthedTerm =
  CheckedTerm ::: CheckedTerm
  | TAbort CheckedTerm
  | TZero
  | TSucc CheckedTerm
  | TNatRec CheckedTerm CheckedTerm CheckedTerm CheckedTerm
  | TUseProof CheckedTerm CheckedTerm CheckedTerm CheckedTerm CheckedTerm
  | TPi SynthedTerm SynthedTerm
  | TBottom
  | TTop
  | TNat
  | TRefine SynthedTerm SynthedTerm
  | TSort Level
  | TVar Int
  | TApp SynthedTerm CheckedTerm
  | TLetS SynthedTerm SynthedTerm
  deriving Show

tSortN :: Int -> SynthedTerm
tSortN = TSort . LevelN

tSortW :: SynthedTerm
tSortW = TSort LevelW

tStar :: SynthedTerm
tStar = tSortN 0

tBox :: SynthedTerm
tBox = tSortN 1

eraseChecked :: CheckedTerm -> Term
eraseChecked (Synthed x) = eraseSynthed x
eraseChecked (TLam x) = Lam (eraseChecked x)
eraseChecked (TAddProof x t p) =
  AddProof (eraseChecked x) (eraseChecked t) (Irrel $ eraseChecked p)
eraseChecked (TLetC x y) = Substed (bindingSubst (eraseSynthed x)) (eraseChecked y)

eraseSynthed :: SynthedTerm -> Term
eraseSynthed (x ::: _) = eraseChecked x
eraseSynthed (TAbort x) = Abort $ Irrel $ eraseChecked x
eraseSynthed (TZero) = Zero
eraseSynthed (TSucc x) = Succ $ eraseChecked x
eraseSynthed (TNatRec t x y n) =
  NatRec (eraseChecked t) (eraseChecked x) (eraseChecked y) (eraseChecked n)
eraseSynthed (TUseProof tx tp x ty y) =
  UseProof (eraseChecked tx) (eraseChecked tp) (eraseChecked x)
  (eraseChecked ty) (eraseChecked y)
eraseSynthed (TPi x y) = Pi (eraseSynthed x) (eraseSynthed y)
eraseSynthed (TBottom) = Bottom
eraseSynthed (TTop) = Top
eraseSynthed (TNat) = Nat
eraseSynthed (TRefine x p) = Refine (eraseSynthed x) (eraseSynthed p)
eraseSynthed (TSort k) = Sort k
eraseSynthed (TVar n) = Var n
eraseSynthed (TApp x y) = App (eraseSynthed x) (eraseChecked y)
eraseSynthed (TLetS x y) = Substed (bindingSubst (eraseSynthed x)) (eraseSynthed y)
