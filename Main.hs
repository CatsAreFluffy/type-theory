module Main where

import NbE
import Parser
import Text.Parsec
import Typechecker
import TypedTerm

demoteMaybe :: Maybe a -> IO a
demoteMaybe (Just a) = return a
demoteMaybe Nothing = fail "Maybe fail"

demoteEither :: Show a => Either a b -> IO b
demoteEither (Right b) = return b
demoteEither (Left a) = fail $ show a

repl :: IO ()
repl = do
  code <- getLine
  st <- demoteEither $ parse expr "<interactive>" code
  print st
  tt <- demoteMaybe $ indexifyS [] st
  print tt
  typ <- demoteMaybe $ synth [] tt
  print typ
  print (normalizeTypeValue [] typ)
  print (normalizeValue [] typ $ eval (eraseSynthed tt) [])
  repl

main :: IO ()
main = putStrLn "Hello, Haskell!" >> repl
