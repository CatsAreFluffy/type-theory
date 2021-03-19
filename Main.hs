module Main where

import Repl

main :: IO ()
main = putStrLn "Hello, Haskell!" >> repl [] []
