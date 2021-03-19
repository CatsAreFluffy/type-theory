module Repl where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Data.Void
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

repl :: [String] -> [Normal] -> ExceptT Void IO ()
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
    handleError :: String -> ExceptT Void IO ()
    handleError s = lift (putStrLn "Error:" >> putStrLn s) >> repl names values
    replIfError :: ExceptT String IO () -> ExceptT Void IO ()
    replIfError e = catchE e handleError

untilItWorks :: ExceptT String IO a -> ExceptT Void IO a
untilItWorks x = do
  r <- lift $ runExceptT x
  case r of
    Right a -> return a
    Left e -> lift (putStrLn "Error:" >> putStrLn e) >> untilItWorks x

replExpr :: [String] -> [Normal] -> SourceTerm -> ExceptT String IO ()
replExpr names values st = do
  processTerm names values st
  catchE (repl names values) absurd

replLine :: [String] -> [Normal] -> String -> SourceTerm -> ExceptT String IO ()
replLine names values s st = do
  n <- processTerm names values st
  lift $ putStrLn $ s ++ " is defined"
  catchE (repl (s:names) (n:values)) absurd

processTerm :: [String] -> [Normal] -> SourceTerm -> ExceptT String IO Normal
processTerm names values st = do
  lift $ putStr "Parsed: " >> print st
  tt <- interpretEither $ indexifyS names st
  lift $ putStr "Indexed: " >> print tt
  typ <- interpretEither $ synth values tt
  lift $ putStr "Type: " >> print typ
  let env = toEnv values
  let term = eval (eraseSynthed tt) env
  lift $ putStr "Normal type: " >> print (normalizeTypeValue env typ)
  lift $ putStr "Normal term: " >> print (normalizeValue env typ term)
  return $ Normal typ term