
module Ast where

data Module = Module [Func]

data Func = Func {
        name        :: String,
        visability  :: Visability,
        args        :: [String],
        body        :: Expr
    }
    deriving Show

data Visability = Public
                | Private
    deriving Show

data Expr = BOp Op Expr Expr
          | Var String
          | Lit32 Integer -- using Integer makes some codegen easier
                          -- (avoiding incidental complexity)
    deriving Show

data Op = Add
        | Subtract
        | Multiply
        | Divide
    deriving Show

