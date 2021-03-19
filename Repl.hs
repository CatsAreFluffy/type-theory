module Repl where

import NbE
import Parser
import Term
import Text.Parsec
import Typechecker
import TypedTerm

demoteMaybe :: Maybe a -> IO a
demoteMaybe (Just a) = return a
demoteMaybe Nothing = fail "Maybe fail"

demoteEither :: Show a => Either a b -> IO b
demoteEither (Right b) = return b
demoteEither (Left a) = fail $ show a

repl :: [String] -> [Normal] -> IO ()
repl names values = do
  code <- getLine
  sl <- demoteEither $ parse line "<interactive>" code
  case sl of
    ExprLine st -> replExpr names values st
    DefLine s st -> replLine names values s st

replExpr :: [String] -> [Normal] -> SourceTerm -> IO ()
replExpr names values st = do
  putStr "Parsed: " >> print st
  tt <- demoteMaybe $ indexifyS names st
  putStr "Indexed: " >> print tt
  typ <- demoteMaybe $ synth values tt
  let env = toEnv values
  let term = eval (eraseSynthed tt) env
  putStr "Type: " >> print typ
  putStr "Normal type: " >> print (normalizeTypeValue env typ)
  putStr "Normal term: " >> print (normalizeValue env typ term)
  repl names values

replLine :: [String] -> [Normal] -> String -> SourceTerm -> IO ()
replLine names values s st = do
  putStr "Parsed: " >> print st
  tt <- demoteMaybe $ indexifyS names st
  putStr "Indexed: " >> print tt
  typ <- demoteMaybe $ synth values tt
  putStr "Type: " >> print typ
  let env = toEnv values
  let term = eval (eraseSynthed tt) env
  putStr "Normal type: " >> print (normalizeTypeValue env typ)
  putStr "Normal term: " >> print (normalizeValue env typ term)
  putStrLn $ s ++ " is defined"
  repl (s:names) (Normal typ term:values)