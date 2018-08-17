
module Ast where

data Module = Module [Func]
    deriving Show

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
          | Let String Expr Expr
          | Case Expr [PatExpr]
          | Call String [Expr]
          | Var String
          | Lit32 Integer -- using Integer makes some codegen easier
                          -- (avoiding incidental complexity)
    deriving Show

data PatExpr = PatExpr Pattern Expr
    deriving Show

data Pattern = Number Integer
             | Wildcard
    deriving Show

data Op = Add
        | Subtract
        | Multiply
        | Divide
        | LessThan
        | LessThanEqualTo
        | GreaterThan
        | GreaterThanEqualTo
        | NotEqual
        | Equal
    deriving Show

