module Parser where

import Control.Applicative ((<**>))
import Data.List (elemIndex)
import Term (Level(LevelN, LevelW, LevelAfterW))
import TypedTerm
import Text.Parsec

data SourceTerm =
  SLam String SourceTerm
  | SApp SourceTerm SourceTerm
  | SVar String
  | SNatLit Int
  | SNatRec String SourceTerm SourceTerm String String SourceTerm SourceTerm
  | SAddProof SourceTerm SourceTerm SourceTerm
  | SUseProof SourceTerm SourceTerm SourceTerm SourceTerm String SourceTerm
  | SPair SourceTerm SourceTerm
  | SProj1 SourceTerm
  | SProj2 SourceTerm
  | SPi String SourceTerm SourceTerm
  | SRefine SourceTerm String SourceTerm
  | SSigma String SourceTerm SourceTerm
  | SSort Level
  | SLet String SourceTerm SourceTerm
  | STyped SourceTerm SourceTerm
  deriving Show

lexeme :: Parsec String st r -> Parsec String st r
lexeme = (<* spaces)

lchar :: Char -> Parsec String st Char
lchar = lexeme . char

lkeyword :: String -> Parsec String st String
lkeyword = lexeme . try . string

ident :: Parsec String st String
ident = lexeme $
  (:) <$> (alphaNum <|> char '_') <*> many (alphaNum <|> oneOf "_'")

expr :: Parsec String st SourceTerm
expr = do
  x <- subexpr
  (lchar ':' *> (STyped x <$> subexpr)) <|> return x

subexpr :: Parsec String st SourceTerm
subexpr = lexeme $ pi' <|> sigma <|> soit <|> lam <|> superfactor

superfactor :: Parsec String st SourceTerm
superfactor = do
  x <- factor
  (lkeyword "->" *> (SPi "_" x <$> subexpr)) <|> return x

factor :: Parsec String st SourceTerm
factor = lexeme $ subfactor <**> postfixes

postfixes :: Parsec String st (SourceTerm -> SourceTerm)
postfixes = (flip (.) <$> postfix <*> postfixes) <|> return id

postfix :: Parsec String st (SourceTerm -> SourceTerm)
postfix = lexeme $ app <|> addproof <|> refine

app :: Parsec String st (SourceTerm -> SourceTerm)
app = lexeme $ flip SApp <$> subfactor

addproof :: Parsec String st (SourceTerm -> SourceTerm)
addproof = lexeme $ (\p t x->SAddProof x t p) <$ lchar '{'
  <*> subexpr <* lchar ':' <*> expr <* lchar '}'

refine :: Parsec String st (SourceTerm -> SourceTerm)
refine = lexeme $ (\n p x->SRefine x n p) <$ lchar '['
  <*> ident <* lchar '.' <*> expr <* lchar ']'

subfactor :: Parsec String st SourceTerm
subfactor =
  sort <|> nat <|> natrec <|> useproof <|> pair
  <|> proj <|> var <|> (lchar '(' *> expr <* lchar ')')

pi' :: Parsec String st SourceTerm
pi' = lexeme $ SPi <$ lchar '^' <*> (try (ident <* lchar ':') <|> return "_")
  <*> expr <* lchar '.' <*> subexpr

sigma :: Parsec String st SourceTerm
sigma = lexeme $ SSigma <$ lchar '&' <*> (try (ident <* lchar ':') <|> return "_")
  <*> expr <* lchar '.' <*> subexpr

soit :: Parsec String st SourceTerm
soit = lexeme $ SLet <$ lchar '|' <*> ident <* lchar '='
  <*> expr <* lchar '.' <*> subexpr

lam :: Parsec String st SourceTerm
lam = lexeme $ lexeme (oneOf "\\!") *> sublam

sublam :: Parsec String st SourceTerm
sublam = lexeme $ (lchar '.' *> subexpr) <|> SLam <$> ident <*> sublam

sort :: Parsec String st SourceTerm
sort = lexeme $
  try (SSort . LevelN <$ char '*' <*> (read <$> many1 digit)) <|>
  try (SSort LevelW <$ string "*w") <|>
  try (SSort LevelAfterW <$ string "*x") <|>
  (SSort (LevelN 0) <$ char '*') <|>
  (SSort (LevelN 1) <$ char '?')

nat :: Parsec String st SourceTerm
nat = lexeme $ try $ (SNatLit . read) <$> many1 digit

natrec :: Parsec String st SourceTerm
natrec = lexeme $
  SNatRec <$ lkeyword "natrec" <* lchar '{'
  <*> (try (ident <* lchar '.') <|> return "_") <*> expr <* lchar ';'
  <*> expr <* lchar ';'
  <*> ident <*> ident <* lchar '.' <*> expr <* lchar ';'
  <*> expr <* optional (lchar ';')
  <* lchar '}'

useproof :: Parsec String st SourceTerm
useproof = lexeme $
  SUseProof <$ lkeyword "useproof" <* lchar '{'
  <*> expr <* lchar ';'
  <*> expr <* lchar ';'
  <*> expr <* lchar ';'
  <*> expr <* lchar ';'
  <*> (try (ident <* lchar '.') <|> return "_")
    <*> expr <* optional (lchar ';')
  <* lchar '}'

pair :: Parsec String st SourceTerm
pair = lexeme $
  SPair <$ lkeyword "pair" <* lchar '{'
  <*> expr <* lchar ';'
  <*> expr <* optional (lchar ';')
  <* lchar '}'

proj :: Parsec String st SourceTerm
proj = lexeme $
  ((SProj1 <$ lkeyword "left") <|> (SProj2 <$ lkeyword "right"))
  <* lchar '{' <*> expr <* lchar '}'

var :: Parsec String st SourceTerm
var = lexeme $ SVar <$> ident

data CodeLine =
  ExprLine SourceTerm
  | DefLine String SourceTerm

line :: Parsec String st CodeLine
line = (try (DefLine <$> ident <* lchar '=') <*> expr) <|> (ExprLine <$> expr)

lookupVar :: String -> [String] -> Either String Int
lookupVar s ss = case elemIndex s ss of
  Just n -> return n
  Nothing -> Left $ "Cannot find var " ++ s ++ " in " ++ show ss

indexifyC :: [String] -> SourceTerm -> Either String CheckedTerm
indexifyC ss (SLam s x) = TLam <$> indexifyC (s:ss) x
indexifyC ss (SAddProof x t p) =
  TAddProof <$> indexifyC ss x <*> indexifyC ss t <*> indexifyC ss p
indexifyC ss (SPair a b) =
  TPair <$> indexifyC ss a <*> indexifyC ss b
indexifyC ss (SLet s x y) = TLetC <$> indexifyS ss x <*> indexifyC (s:ss) y
indexifyC ss x = Synthed <$> indexifyS ss x

indexifyS :: [String] -> SourceTerm -> Either String SynthedTerm
indexifyS ss (SVar s) = TVar <$> lookupVar s ss
indexifyS ss (SNatLit n) =
  TApp <$> (indexifyS ss $ SVar "fromNat") <*> return (fromNat n)
  where
    fromNat 0 = Synthed $ TZero
    fromNat n = Synthed $ TSucc $ fromNat $ n - 1
indexifyS ss (SNatRec n1 t z n2 p s n) =
  TNatRec <$> indexifyC (n1:ss) t <*> indexifyC ss z
  <*> indexifyC (p:n2:ss) s <*> indexifyC ss n
-- the stubbed-out binders are technically unnecessary,
-- and I'll remove them someday probably
indexifyS ss (SUseProof tx tp x ty n1 y) =
  TUseProof <$> indexifyC ss tx <*> indexifyC ss tp <*> indexifyC ss x
  <*> indexifyC ("":ss) ty <*> indexifyC (n1:"":ss) y
indexifyS ss (SProj1 p) = TProj1 <$> indexifyS ss p
indexifyS ss (SProj2 p) = TProj2 <$> indexifyS ss p
indexifyS ss (SPi s t x) = TPi <$> indexifyS ss t <*> indexifyS (s:ss) x
indexifyS ss (SRefine x s p) = TRefine <$> indexifyS ss x <*> indexifyS (s:ss) p
indexifyS ss (SSigma s t x) = TSigma <$> indexifyS ss t <*> indexifyS (s:ss) x
indexifyS ss (SSort k) = return $ TSort k
indexifyS ss (SApp x y) = TApp <$> (indexifyS ss x) <*> (indexifyC ss y)
indexifyS ss (SLet s x y) = TLetS <$> indexifyS ss x <*> indexifyS (s:ss) y
indexifyS ss (STyped x t) = (:::) <$> indexifyC ss x <*> indexifyC ss t
indexifyS _ x = Left $ "Can't synth for " ++ show x
