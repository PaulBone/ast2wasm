
module Wasm where

data Module = Module [Func]

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

data AtomicInstr = Add Type
                 | Sub Type
                 | Mul Type
                 | DivS Type
                 | DivU Type
                 | RemS Type
                 | RemU Type
                 | Call Int
                 | GetLocal Int
                 | ConstI Type Integer
                 | Return
    deriving Show

