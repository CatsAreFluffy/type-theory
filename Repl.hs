module Repl where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Erase
import NbE
import Parser
import System.IO (hFlush, stdout)
import Term
import Text.Parsec
import Typechecker
import TypedTerm

interpretMaybe :: Maybe a -> ExceptT String IO a
interpretMaybe (Just a) = return a
interpretMaybe Nothing = throwE "Maybe fail"

interpretEither :: Either String b -> ExceptT String IO b
interpretEither (Right b) = return b
interpretEither (Left a) = throwE a

repl :: [String] -> [Normal] -> ExceptT a IO b
repl names values = do
  sl <- untilItWorks $ do
    lift $ putStr "> " >> hFlush stdout
    code <- lift getLine
    case parse line "<interactive>" code of
      Left e -> throwE $ show e
      Right x -> return x
  case sl of
    ExprLine st -> replIfError $ replExpr names values st
    DefLine s st -> replIfError $ replLine names values s st
  where
    handleError :: String -> ExceptT a IO b
    handleError s = lift (putStrLn "Error:" >> putStrLn s) >> repl names values
    replIfError :: ExceptT String IO b -> ExceptT a IO b
    replIfError e = catchE e handleError

untilItWorks :: ExceptT String IO b -> ExceptT a IO b
untilItWorks x = do
  r <- lift $ runExceptT x
  case r of
    Right a -> return a
    Left e -> lift (putStrLn "Error:" >> putStrLn e) >> untilItWorks x

replExpr :: [String] -> [Normal] -> SourceTerm -> ExceptT String IO b
replExpr names values st = do
  processTerm names values st
  repl names values

replLine :: [String] -> [Normal] -> String -> SourceTerm -> ExceptT String IO b
replLine names values s st = do
  n <- processTerm names values st
  lift $ putStrLn $ s ++ " is defined"
  repl (s:names) (n:values)

processTerm :: [String] -> [Normal] -> SourceTerm -> ExceptT String IO Normal
processTerm names values st = do
  lift $ putStr "Parsed: " >> print st
  tt <- interpretEither $ indexifyS names st
  lift $ putStr "Indexed: " >> print tt
  typ <- interpretEither $ synth values tt
  let env = toEnv values
  let term = eval (eraseSynthed tt) env
  lift $ putStr "Normal type: " >> print (normalizeTypeValue env typ)
  lift $ putStr "Normal term: " >> print (normalizeValue env typ term)
  let printBLC = do {
    untyp <- interpretEither $ eraseTypedValue typ term (length env);
    lift $ putStrLn $ "BLC: " ++ toBLC (etaReduce untyp);
  }
  catchE printBLC (\e -> lift $ putStrLn $ "BLC failed: " ++ e)
  return $ Normal typ term
  