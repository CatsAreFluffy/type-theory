module Erase where

import NbE
import Term

data UntypedTerm =
  UVar Int
  | ULam UntypedTerm
  | UApp UntypedTerm UntypedTerm
  deriving Show

endsInSort :: Value -> Either String Bool
endsInSort (VPi (VSort k) f) | k > 0 = Left $ "Erasing pi over large sort " ++ show k
endsInSort (VPi a f) = endsInSort $ inst f [Reflect a (NVar 1000)]
endsInSort (VSort _) = Right True
endsInSort (Reflect t _) = Right False

endsInSortN :: Neutral -> Either String Bool
endsInSortN (NVar k) = Right False
endsInSortN (NApp x y) = endsInSortN x

eraseTypedValue :: Value -> Value -> Int -> Either String UntypedTerm
eraseTypedValue (VPi a f) x n = do
  end <- endsInSort a
  case end of
    True -> eraseTypedValue (inst f [irrel]) (vApp x irrel) n
    False -> ULam <$> eraseTypedValue (inst f [fresh]) (vApp x fresh) (n + 1)
    where
      irrel = Reflect a (NVar 10000)
      fresh = Reflect a (NVar n)
eraseTypedValue (VSort k) t n = Left "Erasing a sort"
eraseTypedValue (Reflect _ _) (Reflect _ e) n = eraseNeutral e n

eraseNormal :: Normal -> Int -> Either String UntypedTerm
eraseNormal (Normal t x) = eraseTypedValue t x

eraseNeutral :: Neutral -> Int -> Either String UntypedTerm
eraseNeutral (NVar k) n = return $ UVar $ n - (k + 1)
eraseNeutral (NApp x (Normal y z)) n = do
  end <- endsInSort y
  case end of
    True -> eraseNeutral x n
    False -> UApp <$> (eraseNeutral x n) <*> (eraseTypedValue y z n)

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
