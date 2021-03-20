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
  | TSort Int
  | TVar Int
  | TApp SynthedTerm CheckedTerm
  | TLetS SynthedTerm SynthedTerm
  deriving Show

tstar :: SynthedTerm
tstar = TSort 0

tbox :: SynthedTerm
tbox = TSort 1

eraseChecked :: CheckedTerm -> Term
eraseChecked (Synthed x) = eraseSynthed x
eraseChecked (TLam x) = Lam (eraseChecked x)
eraseChecked (TLetC x y) = Substed (ExtendS IdS (eraseSynthed x)) (eraseChecked y)

eraseSynthed :: SynthedTerm -> Term
eraseSynthed (x ::: _) = eraseChecked x
eraseSynthed (TPi x y) = Pi (eraseSynthed x) (eraseSynthed y)
eraseSynthed (TSort n) = Sort n
eraseSynthed (TVar n) = Var n
eraseSynthed (TApp x y) = App (eraseSynthed x) (eraseChecked y)
eraseSynthed (TLetS x y) = Substed (ExtendS IdS (eraseSynthed x)) (eraseSynthed y)
-- eraseSynthed (TTypedLam _ x) = Lam (eraseSynthed x)