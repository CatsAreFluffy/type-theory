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
checkType c (TLam x) = Left $ show "Lambda " ++ show x ++ " isn't a type"
checkType c (TLetC x y) = do
  tx <- synth c x
  checkType (Normal tx (evalSynthed x c) : c) y

openSort :: Value -> Either String Int
openSort (VSort n) = return n
openSort x = Left $ show x ++ " isn't a sort"

synth :: Context -> SynthedTerm -> Either String Value
-- synth a b | trace ("synth\n" ++ show a ++ '\n':show b) False = undefined
synth c (x ::: t) = do
  checkType c t
  let t' = evalChecked t c
  check c x t'
  return t'
synth c (TPi x y) = do
  tx <- synth c x
  sx <- openSort tx
  sy <- openSort =<< synth (addVar (evalSynthed x c) c) y
  return $ VSort $ case sy of
    -- impredicativity!
    0 -> 0
    _ -> max sx sy
synth x (TSort n) = return $ VSort $ n + 1
synth c (TVar k) = return $ nType $ c !! k
synth c (TApp x y) = do
  t <- synth c x
  case t of
    VPi a f -> do
      check c y a
      return $ inst f [evalChecked y c]
    _ -> Left $ show x ++ " is of type " ++ show t ++", which isn't a pi"
synth c (TLetS x y) = do
  tx <- synth c x
  synth (Normal tx (evalSynthed x c) : c) y