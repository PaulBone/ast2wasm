
module Main where

import Ast
import Codegen
import Wat

ctof = Module [
        Func "id" Public ["x"] $ Var "x",
        Func "ctof" Public ["c"] $
            BOp Add
                (BOp Divide
                    (BOp Multiply (Var "c") (Lit32 9))
                    (Lit32 5))
                (Lit32 32)
    ]

main :: IO ()
main = case codegen ctof of
    Nothing -> putStrLn "Compilation error"
    Just wasm -> putStrLn $ writeWat wasm

