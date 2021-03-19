module Parser where

import Term
import Text.Parsec

data SourceTerm =
  SLam String SourceTerm
  | SApp SourceTerm SourceTerm
  | SVar String
  | SPi String SourceTerm SourceTerm

lexeme :: Parsec String st r -> Parsec String st r
lexeme = (<* spaces)

expr :: Parsec