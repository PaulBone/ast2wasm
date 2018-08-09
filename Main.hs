
module Main where

import System.Exit (die)

import Codegen
import Parse
import Wat

main :: IO ()
main = do input <- getContents
          case compile input of
            Left e -> die $ "Error: " ++ e
            Right wasm -> do putStrLn $ writeWat wasm

compile input = do ast <- parseHL input
                   codegen ast

