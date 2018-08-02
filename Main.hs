
module Main where

import Ast
import Codegen

ctof = Func "ctof" Public ["c"] $ Var "c"


main :: IO ()
main = putStrLn $ show $ codegen $ ctof


