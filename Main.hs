module Main where

import Control.Monad.Trans.Except (runExceptT)
import Data.Void (absurd)
import Repl

main :: IO ()
main = do
  stuff <- runExceptT $ repl baseNames baseValues
  case stuff of
    Left s -> absurd s
    Right _ -> putStrLn "Have fun!"
