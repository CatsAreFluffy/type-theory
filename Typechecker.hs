module Typechecker where

import Control.Monad (guard)
import Data.List (intercalate)
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

echo s x = trace ("echo " ++ s ++ " " ++ show x) x

check :: Context -> CheckedTerm -> Value -> Either String ()
-- check a b c | trace ("check\n" ++ show a ++ '\n':show b ++ '\n':show c) False = undefined
check c (Synthed x) t = do
  t' <- synth c x
  case subtype (quoteType t' lc) (quoteType t lc) of
    True -> Right ()
    False -> Left $ intercalate " "
      [show $ quoteType t lc, "!>=", show $ quoteType t' lc]
  where lc = length c
-- impredicativity!
check c (TPi x y) t@(VSort 0) = checkType c x >>
  check (addVar (evalChecked x c) c) y t
check c (TPi x y) t@(VSort _) = check c x t >>
  check (addVar (evalChecked x c) c) y t
check c (TPi x y) t = Left $ show t ++ " isn't a sort"
check c (TSort n) t@(VSort m)
  | n < m = Right ()
  | otherwise = Left $ "Sort " ++ show n ++ " isn't in sort " ++ show m
check c (TSort n) _ = Left $ "Sort " ++ show n ++ " only inhabits sorts"
check c (TLam x) (VPi a f) = check (addVar a c) x (inst f [fresh])
  where fresh = Reflect a (NVar $ length c)
check c (TLam x) _ = Left $ "Lambda " ++ show x ++ " only inhabits pis"
check c (TLetC x y) t = do
  tx <- synth c x
  check (Normal tx (evalSynthed x c) : c) y t

checkType :: Context -> CheckedTerm -> Either String ()
-- checkType a b | trace ("checkType\n" ++ show a ++ '\n':show b) False = undefined
checkType c (Synthed x) = do
  t <- synth c x
  case t of
    VSort _ -> Right ()
    x -> Left $ show x ++ " isn't a sort"
checkType c (TPi x y) = checkType c x >>
  checkType (addVar (evalChecked x c) c) y
checkType c (TSort n) = Right ()
checkType c (TLam x) = Left $ show "Lambda " ++ show x ++ " isn't a type"

synth :: Context -> SynthedTerm -> Either String Value
-- synth a b | trace ("synth\n" ++ show a ++ '\n':show b) False = undefined
synth c (x ::: t) = do
  checkType c t
  let t' = evalChecked t c
  check c x t'
  return t'
synth c (TVar k) = Right $ nType $ c !! k
synth c (TApp x y) = do
  t <- synth c x
  case t of
    VPi a f -> do
      check c y a
      return $ inst f [evalChecked y c]
    x -> Left $ show x ++ " doesn't have a pi type"
synth c (TLetS x y) = do
  tx <- synth c x
  synth (Normal tx (evalSynthed x c) : c) y