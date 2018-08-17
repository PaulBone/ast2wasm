
module Wasm where

data Module = Module [Func]

data Func = Func {
        name    :: Maybe String,
        type_   :: Type,
        args    :: [Type],
        locals  :: [Type],
        body    :: [Instr]
    }
    deriving Show

data Type = I32
    deriving Show

data Instr = Atomic AtomicInstr
           | If [Type] [Instr] [Instr]
    deriving Show

data AtomicInstr = Add Type
                 | Sub Type
                 | Mul Type
                 | DivS Type
                 | DivU Type
                 | RemS Type
                 | RemU Type
                 | LtS Type
                 | LtU Type
                 | LeS Type
                 | LeU Type
                 | GtS Type
                 | GtU Type
                 | GeS Type
                 | GeU Type
                 | Ne Type
                 | Eq Type
                 | Call Int
                 | GetLocal Int
                 | SetLocal Int
                 | ConstI Type Integer
                 | Return
    deriving Show

