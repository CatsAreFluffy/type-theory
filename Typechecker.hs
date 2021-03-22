module Typechecker where

import Control.Monad (guard)
import Data.List (intercalate)
import Data.Functor (($>))
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

closureChecked :: CheckedTerm -> Context -> Closure
closureChecked x c = Closure (eraseChecked x) (toEnv c)

echo s x = trace ("echo " ++ s ++ " " ++ show x) x

check :: Context -> CheckedTerm -> Value -> Either String ()
-- check a b c | trace ("check\n" ++ show a ++ '\n':show b ++ '\n':show c) False = undefined
check c (Synthed x) t = do
  t' <- synth c x
  case subtype t' t lc of
    True -> return ()
    False -> Left $ intercalate " "
      [show $ quoteType t lc, "!>=", show $ quoteType t' lc]
  where lc = length c
check c (TLam x) VTop = check (addVar VBottom c) x VTop
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
    VSort _ -> return ()
    VBottom -> return ()
    x -> Left $ show x ++ " isn't a sort"
checkType c (TLam x) = Left $ show "Lambda " ++ show x ++ " isn't a type"
checkType c (TLetC x y) = do
  tx <- synth c x
  checkType (Normal tx (evalSynthed x c) : c) y

openSort :: Value -> Either String Level
openSort (VSort k) = return k
openSort (VBottom) = return $ LevelN 0
openSort x = Left $ show x ++ " isn't a sort"

synth :: Context -> SynthedTerm -> Either String Value
-- synth a b | trace ("synth\n" ++ show a ++ '\n':show b) False = undefined
synth c (x ::: t) = do
  checkType c t
  let t' = evalChecked t c
  check c x t'
  return t'
synth c (TZero) = return $ VNat
synth c (TSucc x) = check c x VNat $> VNat
synth c (TNatRec t x y n) = do
  check c n VNat
  checkType (addVar VNat c) t
  let t' = closureChecked t c
  let tSuccVar = (inst t' [VSucc $ natVarN])
  check c x (inst t' [VZero])
  check (addVar tSuccVar $ addVar VNat c) y (inst t' [natVarN])
  return $ inst t' [evalChecked n c]
  where natVarN = Reflect VNat $ NVar $ length c
synth c (TPi x y) = do
  tx <- synth c x
  sx <- openSort tx
  sy <- openSort =<< synth (addVar (evalSynthed x c) c) y
  return $ VSort $ case sy of
    -- impredicativity!
    LevelN 0 -> LevelN 0
    _ -> max sx sy
synth x (TBottom) = return $ vStar
-- it's contractible so this is probably fine
synth x (TTop) = return $ vStar
synth x (TNat) = return $ vStar
synth x (TSort (LevelN n)) = return . VSort . LevelN $ n + 1
synth x (TSort LevelW) = return $ VSort LevelAfterW
synth x (TSort LevelAfterW) = Left $ "*x has no type"
synth c (TVar k) = return $ nType $ c !! k
synth c (TApp x y) = do
  t <- synth c x
  case t of
    VPi a f -> do
      check c y a
      return $ inst f [evalChecked y c]
    -- The smallest function type is Top->Bottom
    VBottom -> do
      check c y VTop
      return VBottom
    _ -> Left $ show x ++ " is of type " ++ show t ++", which isn't a pi"
synth c (TLetS x y) = do
  tx <- synth c x
  synth (Normal tx (evalSynthed x c) : c) y
