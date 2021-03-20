module Erase where

import NbE
import Term

data UntypedTerm =
  UVar Int
  | ULam UntypedTerm
  | UApp UntypedTerm UntypedTerm
  deriving Show

finalSort :: Value -> Either String (Maybe Int)
finalSort (VPi a f) = do
  s <- finalSort a
  case s of
    Just k | k > 0 -> Left $ "Erasing pi over large sort " ++ show k
    _ -> finalSort $ inst f [Reflect a (NVar 1000)]
finalSort (VSort k) = Right $ Just k
finalSort (Reflect t _) = Right Nothing

finalSortN :: Neutral -> Either String (Maybe Int)
finalSortN (NVar k) = Right Nothing
finalSortN (NApp x y) = finalSortN x

eraseTypedValue :: Value -> Value -> Int -> Either String UntypedTerm
eraseTypedValue (VPi a f) x n = do
  end <- finalSort a
  case end of
    Just 0 -> eraseTypedValue (inst f [irrel]) (vApp x irrel) n
    Just k -> Left $ "Erasing pi over large sort " ++ show k
    Nothing -> ULam <$> eraseTypedValue (inst f [fresh]) (vApp x fresh) (n + 1)
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
  end <- finalSort y
  case end of
    Just 0 -> eraseNeutral x n
    Just k -> Left $ "Erasing pi over large sort " ++ show k
    Nothing -> UApp <$> (eraseNeutral x n) <*> (eraseTypedValue y z n)

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
