module Erase where

import NbE
import Term

data UntypedTerm =
  UVar Int
  | ULam UntypedTerm
  | UApp UntypedTerm UntypedTerm
  deriving Show

uIdentity :: UntypedTerm
uIdentity = ULam $ UVar 0

finalSort :: Value -> Either String (Maybe Level)
finalSort (VAbort x) = finalSort x
finalSort (VAddProof t _ _) = finalSort t
finalSort (VPi a f) = do
  s <- finalSort a
  case s of
    Just k | k > LevelN 0 -> Left $ "Erasing pi over large sort " ++ show s
    _ -> finalSort $ inst f [Reflect a (NVar 1000)]
finalSort (VRelevantBottom) = return $ Just $ LevelN 0
finalSort (VBottom) = return Nothing -- $ Just $ LevelN 0
finalSort (VTop) = return $ Just $ LevelN 0
finalSort (VNat) = return $ Just $ LevelN 0
finalSort (VRefine t _) = finalSort t
finalSort (VSort k) = return $ Just k
finalSort (Reflect t _) = return Nothing

finalSortN :: Neutral -> Either String (Maybe Level)
finalSortN (NVar k) = return Nothing
finalSortN (NApp x y) = finalSortN x
finalSortN (NNatRec n t x y) = finalSort $ inst t [Reflect VNat $ NVar 1000]
finalSortN (NUseProof' e tx tp ty y) = finalSort $
  inst ty [Reflect tp $ NVar 1000, Reflect tx $ NVar 1000]

eraseTypedValue :: Value -> Value -> Int -> Either String UntypedTerm
eraseTypedValue (VAddProof t _ _) x n = eraseTypedValue t x n
eraseTypedValue (VPi a f) x n = do
  end <- finalSort a
  case end of
    Just (LevelN 0) -> eraseTypedValue (inst f [irrel]) (vApp x irrel) n
    Just k -> Left $ "Erasing pi over large sort " ++ show k
    Nothing -> ULam <$> eraseTypedValue (inst f [fresh]) (vApp x fresh) (n + 1)
    where
      irrel = Reflect a (NVar 10000)
      fresh = Reflect a (NVar n)
eraseTypedValue (VBottom) x n =
  eraseTypedValue VRelevantBottom (vElimAbort x) n
eraseTypedValue (VRelevantBottom) (VAddProof x _ _) n =
  eraseTypedValue VRelevantBottom x n
eraseTypedValue (VRelevantBottom) (VAbort x) n =
  eraseTypedValue VRelevantBottom x n
eraseTypedValue (VRelevantBottom) (Reflect _ e) n = eraseNeutral e n
-- I'm not entirely sure about this one
eraseTypedValue (VTop) t n = return $ UVar 0
eraseTypedValue (VNat) t n = Left "Erasing Nats is unimplemented"
eraseTypedValue (VRefine t _) x n = eraseTypedValue t x n
eraseTypedValue (VSort k) t n = Left "Erasing a sort"
eraseTypedValue (Reflect _ _) (Reflect _ e) n = eraseNeutral e n

eraseNormal :: Normal -> Int -> Either String UntypedTerm
eraseNormal (Normal t x) = eraseTypedValue t x

eraseNeutral :: Neutral -> Int -> Either String UntypedTerm
eraseNeutral (NVar k) n = return $ UVar $ n - (k + 1)
eraseNeutral (NApp x (Normal y z)) n = do
  end <- finalSort y
  case end of
    Just (LevelN 0) -> eraseNeutral x n
    Just k -> Left $ "Erasing pi over large sort " ++ show k
    Nothing -> UApp <$> (eraseNeutral x n) <*> (eraseTypedValue y z n)
eraseNeutral (NNatRec _ _ _ _) n = Left "Erasing NatRecs is unimplemented"
eraseNeutral (NUseProof' e tx tp ty y) n = Left "Erasing UseProofs is unimplemented"

deleteVar :: UntypedTerm -> Int -> Maybe UntypedTerm
deleteVar (UVar n) m
  | n == m = Nothing
  | n > m = Just $ UVar $ n - 1
  | otherwise = Just $ UVar n
deleteVar (ULam x) m = ULam <$> deleteVar x (m + 1)
deleteVar (UApp x y) m = UApp <$> deleteVar x m <*> deleteVar y m

etaReduce :: UntypedTerm -> UntypedTerm
etaReduce (UVar n) = UVar n
etaReduce (ULam x)
  | UApp x' (UVar 0) <- etaReduce x, Just x' <- deleteVar x' 0 = x'
  | otherwise = ULam $ etaReduce x
etaReduce (UApp x y) = UApp (etaReduce x) (etaReduce y)

toBLC :: UntypedTerm -> String
toBLC (UVar n) = replicate (n + 1) '1' ++ "0"
toBLC (ULam x) = "00" ++ toBLC x
toBLC (UApp x y) = "01" ++ toBLC x ++ toBLC y

-- toBLC :: UntypedTerm -> String
-- toBLC = show
