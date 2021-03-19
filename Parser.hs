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
ident = lexeme $ many1 (alphaNum <|> char '_')

expr :: Parsec String st SourceTerm
expr = do
  x <- subexpr
  (lchar ':' *> (STyped x <$> subexpr)) <|> return x

subexpr :: Parsec String st SourceTerm
subexpr = lexeme $ pi' <|> soit <|> lam <|> superfactor

superfactor :: Parsec String st SourceTerm
superfactor = do
  x <- factor
  (lexeme (string "->") *> (SPi "_" x <$> subexpr)) <|> return x

factor :: Parsec String st SourceTerm
factor = lexeme $ chainl1 subfactor (return SApp)

subfactor :: Parsec String st SourceTerm
subfactor = sort <|> var <|> (lchar '(' *> expr <* lchar ')')

pi' :: Parsec String st SourceTerm
pi' = lexeme $ SPi <$ lchar '^' <*> (try (ident <* lchar ':') <|> return "_")
  <*> expr <* lchar '.' <*> subexpr

soit :: Parsec String st SourceTerm
soit = lexeme $ SLet <$ lchar '|' <*> ident <* lchar '='
  <*> expr <* lchar '.' <*> subexpr

lam :: Parsec String st SourceTerm
lam = lexeme $ lexeme (oneOf "\\!") *> sublam

sublam :: Parsec String st SourceTerm
sublam = lexeme $ (lchar '.' *> subexpr) <|> SLam <$> ident <*> sublam

sort :: Parsec String st SourceTerm
sort = lexeme $ try (SSort <$ char '*' <*> (read <$> many1 digit)) <|> (SSort 0 <$ char '*') <|> (SSort 1 <$ char '?')

var :: Parsec String st SourceTerm
var = lexeme $ SVar <$> ident

data CodeLine =
  ExprLine SourceTerm
  | DefLine String SourceTerm

line :: Parsec String st CodeLine
line = (try (DefLine <$> ident <* lchar '=') <*> expr) <|> (ExprLine <$> expr)

lookupVar :: String -> [String] -> Either String Int
lookupVar s ss = case elemIndex s ss of
  Just n -> Right n
  Nothing -> Left $ "Cannot find var " ++ s

indexifyC :: [String] -> SourceTerm -> Either String CheckedTerm
indexifyC ss (SPi s t x) = TPi <$> indexifyC ss t <*> indexifyC (s:ss) x
indexifyC ss (SSort n) = return $ TSort n
indexifyC ss (SLam s x) = TLam <$> indexifyC (s:ss) x
indexifyC ss (SLet s x y) = TLetC <$> indexifyS ss x <*> indexifyC (s:ss) y
indexifyC ss x = Synthed <$> indexifyS ss x

indexifyS :: [String] -> SourceTerm -> Either String SynthedTerm
indexifyS ss (SVar s) = TVar <$> lookupVar s ss
indexifyS ss (SApp x y) = TApp <$> (indexifyS ss x) <*> (indexifyC ss y)
indexifyS ss (SLet s x y) = TLetS <$> indexifyS ss x <*> indexifyS (s:ss) y
indexifyS ss (STyped x t) = (:::) <$> indexifyC ss x <*> indexifyC ss t
-- since this is what you usually want anyway
indexifyS ss (SSort n) = return $ TSort n ::: TSort (n + 1)
indexifyS _ x = Left $ "Can't synth for " ++ show x