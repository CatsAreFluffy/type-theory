module TypedTerm where

import Term

data CheckedTerm =
  Synthed SynthedTerm
  | TPi CheckedTerm CheckedTerm
  | TSort Int
  | TLam CheckedTerm
  deriving Show

data SynthedTerm =
  CheckedTerm ::: CheckedTerm
  | TVar Int
  | TApp SynthedTerm CheckedTerm
  -- | TTypedLam CheckedTerm SynthedTerm
  deriving Show

eraseChecked :: CheckedTerm -> Term
eraseChecked (Synthed x) = eraseSynthed x
eraseChecked (TPi x y) = Pi (eraseChecked x) (eraseChecked y)
eraseChecked (TSort n) = Sort n
eraseChecked (TLam x) = Lam (eraseChecked x)

eraseSynthed :: SynthedTerm -> Term
eraseSynthed (x ::: _) = eraseChecked x
eraseSynthed (TVar n) = Var n
eraseSynthed (TApp x y) = App (eraseSynthed x) (eraseChecked y)
-- eraseSynthed (TTypedLam _ x) = Lam (eraseSynthed x)