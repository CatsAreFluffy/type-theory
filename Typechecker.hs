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
      [show $ quoteType t' lc, "!<=", show $ quoteType t lc]
  where lc = length c
check c (TLam x) t = do
  (a, f) <- maxPiSubtype (toEnv c) t
  let fresh = Reflect a (NVar $ length c)
  check (addVar a c) x (inst f [fresh])
-- check c v@(TLam _) (VAddProof t _ _) = check c v t
-- check c (TLam x) VTop = check (addVar VBottom c) x VTop
-- check c (TLam x) (VPi a f) = check (addVar a c) x (inst f [fresh])
--   where fresh = Reflect a (NVar $ length c)
-- check c (TLam x) _ = Left $ "Lambda " ++ show x ++ " only inhabits pis"
check c v@(TAddProof _ _ _) (VAddProof t _ _) = check c v t
-- check c (TAddProof x t p) VTop = error "Addproof top"
check c (TAddProof x t p) tt@(VRefine tx tp) = do
  check c x tx
  let t' = evalChecked t c
  check c p t'
  let x' = evalChecked x c
  case subtype t' (inst tp [x']) (length c) of
    True -> return ()
    False -> check c x tt
check c (TAddProof x pt p) t = do
  check c x t
  checkType c pt
  let pt' = evalChecked pt c
  check c p (vSquash pt')
check c (TPair x y) t = do
  (a, f) <- maxSigSubtype (toEnv c) t
  check c x a
  let x' = evalChecked x c
  check (Normal a x':c) y (inst f [x'])
check c (TLetC x y) t = do
  tx <- synth c x
  check (Normal tx (evalSynthed x c) : c) y t

checkType :: Context -> CheckedTerm -> Either String ()
-- checkType a b | trace ("checkType\n" ++ show a ++ '\n':show b) False = undefined
checkType c (Synthed x) = do
  t <- synth c x
  openSort t >> return ()
checkType c (TLam x) = Left $ show "Lambda " ++ show x ++ " isn't a type"
checkType c (TAddProof x pt p) = do
  checkType c x
  checkType c pt
  let pt' = evalChecked pt c
  check c p (vSquash pt')
checkType c (TPair a b) =
  Left $ show "Pair " ++ show a ++ ", " ++ show b ++ " isn't a type"
checkType c (TLetC x y) = do
  tx <- synth c x
  checkType (Normal tx (evalSynthed x c) : c) y

checkProofIn :: Context -> Value -> Value -> Value -> Either String ()
-- checkProofIn _ _ _ t | trace ("t" ++ show t) False = undefined
checkProofIn c tp x (VAddProof t _ _) = checkProofIn c tp x t
checkProofIn c tp x (VBottom) = return ()
checkProofIn c tp x (VRefine t tp') = do
  let c' = addVar t c
  let fresh = Reflect t $ NVar $ length c
  case subtype tp (inst tp' [x]) (length c') of
    True -> return ()
    False -> checkProofIn c tp x t
checkProofIn c tp _ _ = Left $ "Couldn't find a proof of " ++ show tp

openSort :: Value -> Either String Level
openSort (VAddProof x _ _) = openSort x
openSort (VRefine t _) = openSort t
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
synth c (TAbort x) = check c x VBottom $> VBottom
synth c (TZero) = return $ VNat
synth c (TSucc x) = check c x VNat $> VNat
synth c (TNatRec t x y n) = do
  check c n VNat
  checkType (addVar VNat c) t
  let t' = closureChecked t c
  let tSuccVar = (inst t' [VSucc $ natVarN])
  check c x (inst t' [VZero])
  check (addVar tSuccVar $ addVar VNat c) y (inst t' [VSucc $ natVarN])
  return $ inst t' [evalChecked n c]
  where natVarN = Reflect VNat $ NVar $ length c
-- useproof{*;*{Top:*}:?[p.*];t.?[p.*];x p.x}
-- \x.useproof{Top[x.Bottom];Bottom;x;t.Bottom;t p.p} : Top[x.Bottom]->Bottom
synth c (TUseProof tx tp x ty y) = do
  checkType c tx
  let tx' = evalChecked tx c
  check c x tx'
  let x' = evalChecked x c
  -- checkType (addVar (evalSynthed x c) c) p
  checkType c tp
  let tp' = evalChecked tp c
  checkProofIn c tp' x' tx'
  checkType (addVar tx' c) ty
  -- let x' = Normal tx $ evalSynthed x c
  let ty' = closureChecked ty c
  let fresh = Reflect tx' $ NVar $ length c
  let tyf = inst ty' [fresh]
  check (addVar tp' $ addVar tx' c) y tyf
  let p1 = Reflect tp' $ NVar $ length c + 1
  let p2 = Reflect tp' $ NVar $ length c + 2
  let y' = closureChecked y c
  let y1 = inst y' [p1,fresh]
  let y2 = inst y' [p2,fresh]
  case valueEq tyf y1 y2 (length c + 3) of
    True -> return ()
    False -> Left $ "Coherence failure in " ++ show (TUseProof tx tp x ty y)
  return $ inst ty' [evalChecked x c]
synth c (TProj1 x) = do
  tx <- synth c x
  (a, _) <- minSigSupertype (toEnv c) tx
  return a
synth c (TProj2 x) = do
  tx <- synth c x
  (a, f) <- minSigSupertype (toEnv c) tx
  return $ inst f [evalSynthed (TProj1 x) c]
synth c (TGetProof tp x) = do
  checkType c tp
  let tp' = evalChecked tp c
  tx <- synth c x
  let x' = evalSynthed x c
  checkProofIn c tp' x' tx
  return $ vSquash tp'
synth c (TUseSquash p ty y) = do
  checkType c ty
  let ty' = evalChecked ty c
  tsp <- synth c p
  let tp = vUnSquash tsp
  let fresh = Reflect tp $ NVar $ length c
  check (addVar tp c) y ty'
  let y' = closureChecked y c
  let fresh2 = Reflect tp $ NVar $ length c + 1
  case valueEq ty' (inst y' [fresh]) (inst y' [fresh2]) (length c + 2) of
    True -> return ()
    False -> Left $ "Coherence failure in " ++ show (TUseSquash p ty y)
  return ty'
synth c (TPi x y) = do
  tx <- synth c x
  sx <- openSort tx
  sy <- openSort =<< synth (addVar (evalSynthed x c) c) y
  return $ VSort $ case sy of
    -- impredicativity!
    LevelN 0 -> LevelN 0
    _ -> max sx sy
synth c (TBottom) = return vStar
synth c (TTop) = return vStar
synth c (TNat) = return vStar
synth c (TRefine t p) = do
  tt <- synth c t
  openSort tt
  tp <- synth (addVar (evalSynthed t c) c) p
  openSort tp
  return tt
synth c (TSigma x y) = do
  tx <- synth c x
  sx <- openSort tx
  sy <- openSort =<< synth (addVar (evalSynthed x c) c) y
  -- no impredicativity here
  return $ VSort $ max sx sy
synth c (TSquash x) = do
  tx <- synth c x
  openSort tx
  return vStar
synth c (TSort (LevelN n)) = return . VSort . LevelN $ n + 1
synth c (TSort LevelW) = return $ VSort LevelAfterW
synth c (TSort LevelAfterW) = Left $ "*x has no type"
synth c (TVar k) = return $ nType $ c !! k
synth c (TApp x y) = do
  t <- synth c x 
  (a, f) <- minPiSupertype (toEnv c) t
  check c y a
  return $ inst f [evalChecked y c]
synth c (TLetS x y) = do
  tx <- synth c x
  synth (Normal tx (evalSynthed x c) : c) y
