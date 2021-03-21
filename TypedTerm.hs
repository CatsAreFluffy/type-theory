module TypedTerm where

import Term

data CheckedTerm =
  Synthed SynthedTerm
  | TLam CheckedTerm
  | TLetC SynthedTerm CheckedTerm
  deriving Show

data SynthedTerm =
  CheckedTerm ::: CheckedTerm
  | TPi SynthedTerm SynthedTerm
  | TSort Level
  | TVar Int
  | TApp SynthedTerm CheckedTerm
  | TLetS SynthedTerm SynthedTerm
  deriving Show

tSortN :: Int -> SynthedTerm
tSortN = TSort . LevelN

tSortW :: SynthedTerm
tSortW = TSort LevelW

tstar :: SynthedTerm
tstar = tSortN 0

tbox :: SynthedTerm
tbox = tSortN 1

eraseChecked :: CheckedTerm -> Term
eraseChecked (Synthed x) = eraseSynthed x
eraseChecked (TLam x) = Lam (eraseChecked x)
eraseChecked (TLetC x y) = Substed (ExtendS IdS (eraseSynthed x)) (eraseChecked y)

eraseSynthed :: SynthedTerm -> Term
eraseSynthed (x ::: _) = eraseChecked x
eraseSynthed (TPi x y) = Pi (eraseSynthed x) (eraseSynthed y)
eraseSynthed (TSort k) = Sort k
eraseSynthed (TVar n) = Var n
eraseSynthed (TApp x y) = App (eraseSynthed x) (eraseChecked y)
eraseSynthed (TLetS x y) = Substed (ExtendS IdS (eraseSynthed x)) (eraseSynthed y)
-- eraseSynthed (TTypedLam _ x) = Lam (eraseSynthed x)