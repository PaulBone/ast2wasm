
module Ast where

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

data Expr = Var String
          | Lit32 Int
    deriving Show

