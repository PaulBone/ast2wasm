
module Main where

import Ast
import Codegen
import Wat

ctof = Func "ctof" Public ["c"] $ Var "c"


main :: IO ()
main = case codegen ctof of
    Nothing -> putStrLn "Compilation error"
    Just wasm -> putStrLn $ writeWat wasm

