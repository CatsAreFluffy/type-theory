module Typechecker where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import NbE
import Term
import TypedTerm

type Context = [Normal]

addVar :: Value -> Context -> Context
addVar x c = (Normal x $ Reflect x $ NVar $ length c) : c

toEnv :: Context -> Env
toEnv = map nVal

evalSynthed :: SynthedTerm -> Context -> Value
evalSynthed x c = eval (eraseSynthed x) (toEnv c)

evalChecked :: CheckedTerm -> Context -> Value
evalChecked x c = eval (eraseChecked x) (toEnv c)

check :: Context -> CheckedTerm -> Value -> Bool
--check a b c | trace ("check\n" ++ show a ++ '\n':show b ++ '\n':show c) False = undefined
check c (Synthed x) t = fromMaybe False $ do
  t' <- synth c x
  return $ quoteType t (length c) == quoteType t' (length c)
-- impredicativity!
check c (TPi x y) t@(VSort 0) = checkType c x &&
  check (addVar (evalChecked x c) c) y t
check c (TPi x y) t@(VSort _) = check c x t &&
  check (addVar (evalChecked x c) c) y t
check c (TPi x y) _ = False
check c (TSort n) t@(VSort m) = n < m
check c (TSort _) _ = False
check c (TLam x) (VPi a f) = check (addVar a c) x (inst f [fresh])
  where fresh = Reflect a (NVar $ length c)
check c (TLam _) _ = False

checkType :: Context -> CheckedTerm -> Bool
checkType c (Synthed x) = case synth c x of
  Just (VSort _) -> True
  _ -> False
checkType c (TPi x y) = checkType c x &&
  checkType (addVar (evalChecked x c) c) y
checkType c (TSort n) = True
checkType c (TLam _) = False

synth :: Context -> SynthedTerm -> Maybe Value
synth c (x ::: t) = do
  guard (checkType c t)
  let t' = evalChecked t c
  guard (check c x t')
  return t'
synth c (TVar k) = Just $ nType $ c !! k
synth c (TApp x y) = do
  (VPi a f) <- synth c x
  guard $ check c y a
  return $ inst f [evalChecked y c]