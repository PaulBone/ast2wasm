
module Wasm where

data Func = Func {
        name    :: Maybe String,
        type_   :: Type,
        args    :: [Type],
        body    :: [Instr]
    }
    deriving Show

data Type = I32
    deriving Show

data Instr = Atomic AtomicInstr
    deriving Show

data AtomicInstr = GetLocal Int
                 | ConstI Type Integer
                 | Return
    deriving Show

