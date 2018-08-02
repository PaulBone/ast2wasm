
module Wasm where

data Func = Func {
        type_ :: Type,
        locals :: [Type],
        body :: Instr
    }
    deriving Show

data Type = I32
    deriving Show

data Instr = Atomic AtomicInstr
    deriving Show

data AtomicInstr = GetLocal Int
                 | Return
    deriving Show

