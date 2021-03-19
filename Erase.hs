module Erase where

import NbE
import Term

data UntypedTerm =
  UVar Int
  | ULam UntypedTerm
  | UApp UntypedTerm UntypedTerm
  deriving Show

eraseTypedValue :: Value -> Value -> Int -> Either String UntypedTerm
eraseTypedValue (VPi (VSort 0) f) x n =
  eraseTypedValue (inst f [fresh]) (vApp x fresh) n
  where fresh = Reflect (VSort 0) (NVar undefined)
eraseTypedValue (VPi (VSort k) f) x n = Left $ "Erasing pi over large sort " ++ show k
eraseTypedValue (VPi a f) x n = ULam <$>
  eraseTypedValue (inst f [fresh]) (vApp x fresh) (n + 1)
  where fresh = Reflect a (NVar n)
eraseTypedValue (VSort k) t n = Left "Erasing a sort"
eraseTypedValue (Reflect _ _) (Reflect _ e) n = eraseNeutral e n

eraseNormal :: Normal -> Int -> Either String UntypedTerm
eraseNormal (Normal t x) = eraseTypedValue t x

eraseNeutral :: Neutral -> Int -> Either String UntypedTerm
eraseNeutral (NVar k) n = return $ UVar $ n - (k + 1)
eraseNeutral (NApp x (Normal (VSort _) _)) n = eraseNeutral x n
eraseNeutral (NApp x y) n = UApp <$> (eraseNeutral x n) <*> (eraseNormal y n)

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
-- toBLC (UVar n) = "V" ++ show n
-- toBLC (ULam x) = "L" ++ toBLC x
-- toBLC (UApp x y) = "A" ++ toBLC x ++ toBLC y
