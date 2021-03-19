module TypedTerm where

import Term

data CheckedTerm =
  Synthed SynthedTerm
  | TPi CheckedTerm CheckedTerm
  | TSort Int
  | TLam CheckedTerm
  | TLetC SynthedTerm CheckedTerm
  deriving Show

tstar :: CheckedTerm
tstar = TSort 0

tbox :: CheckedTerm
tbox = TSort 1

data SynthedTerm =
  CheckedTerm ::: CheckedTerm
  | TVar Int
  | TApp SynthedTerm CheckedTerm
  | TLetS SynthedTerm SynthedTerm
  -- | TTypedLam CheckedTerm SynthedTerm
  deriving Show

eraseChecked :: CheckedTerm -> Term
eraseChecked (Synthed x) = eraseSynthed x
eraseChecked (TPi x y) = Pi (eraseChecked x) (eraseChecked y)
eraseChecked (TSort n) = Sort n
eraseChecked (TLam x) = Lam (eraseChecked x)
eraseChecked (TLetC x y) = Substed (ExtendS IdS (eraseSynthed x)) (eraseChecked y)

eraseSynthed :: SynthedTerm -> Term
eraseSynthed (x ::: _) = eraseChecked x
eraseSynthed (TVar n) = Var n
eraseSynthed (TApp x y) = App (eraseSynthed x) (eraseChecked y)
eraseSynthed (TLetS x y) = Substed (ExtendS IdS (eraseSynthed x)) (eraseSynthed y)
-- eraseSynthed (TTypedLam _ x) = Lam (eraseSynthed x)