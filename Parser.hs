module Parser where

import Data.List (elemIndex)
import TypedTerm
import Text.Parsec

data SourceTerm =
  SLam String SourceTerm
  | SApp SourceTerm SourceTerm
  | SVar String
  | SPi String SourceTerm SourceTerm
  | SSort Int
  | SLet String SourceTerm SourceTerm
  | STyped SourceTerm SourceTerm
  deriving Show

lexeme :: Parsec String st r -> Parsec String st r
lexeme = (<* spaces)

lchar :: Char -> Parsec String st Char
lchar = lexeme . char

ident :: Parsec String st String
ident = lexeme $ many1 alphaNum

expr :: Parsec String st SourceTerm
expr = lexeme $ chainl1 factor (return SApp)

factor :: Parsec String st SourceTerm
factor = lexeme $ pi' <|> soit <|> lam <|> sort <|> var <|> typed <|> between (lexeme $ char '(') (lexeme $ char ')') expr

pi' :: Parsec String st SourceTerm
pi' = lexeme $ SPi <$ lchar '^' <*> ident <* lchar ':'
  <*> expr <* lchar '.' <*> expr

soit :: Parsec String st SourceTerm
soit = lexeme $ SLet <$ lchar '|' <*> ident <* lchar '='
  <*> expr <* lchar '.' <*> expr

lam :: Parsec String st SourceTerm
lam = lexeme $ SLam <$ lexeme (oneOf "\\!") <*> ident <* lchar '.' <*> expr

sort :: Parsec String st SourceTerm
sort = lexeme $ (SSort 0 <$ char '*') <|> (SSort 1 <$ char '?')

var :: Parsec String st SourceTerm
var = lexeme $ SVar <$> ident

typed :: Parsec String st SourceTerm
typed = lexeme $ STyped <$ lchar '[' <*> expr <* lchar ':' <*> expr <* lchar ']'

indexifyC :: [String] -> SourceTerm -> Maybe CheckedTerm
indexifyC ss (SPi s t x) = TPi <$> indexifyC ss t <*> indexifyC (s:ss) x
indexifyC ss (SSort n) = return $ TSort n
indexifyC ss (SLam s x) = TLam <$> indexifyC (s:ss) x
indexifyC ss (SLet s x y) = TLetC <$> indexifyS ss x <*> indexifyC (s:ss) y
indexifyC ss x = Synthed <$> indexifyS ss x

indexifyS :: [String] -> SourceTerm -> Maybe SynthedTerm
indexifyS ss (SVar s) = TVar <$> elemIndex s ss
indexifyS ss (SApp x y) = TApp <$> (indexifyS ss x) <*> (indexifyC ss y)
indexifyS ss (SLet s x y) = TLetS <$> indexifyS ss x <*> indexifyS (s:ss) y
indexifyS ss (STyped x t) = (:::) <$> indexifyC ss x <*> indexifyC ss t
indexifyS _ _ = Nothing
