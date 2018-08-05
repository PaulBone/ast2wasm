
module Wat (writeWat) where

import qualified Data.Text as Text
import Data.SCargot.Print
import Data.SCargot.Repr

import qualified Wasm as W

writeWat :: W.Func -> String
writeWat = Text.unpack . sexpr2Text .  wasm2SExpr

wasm2SExpr (W.Func name0 rettype0 params0 instrs0) =
    WFSList $ [WFSAtom Func] ++ name ++ params ++ [rettype, instrs]
    where name = case name0 of
            Nothing -> []
            Just n -> [WFSList [WFSAtom Export, WFSAtom (Str n)]]
          params = map (\p -> WFSList [WFSAtom Param, type2SExpr p])
                       params0
          rettype = WFSList [WFSAtom Result, type2SExpr rettype0]
          instrs = WFSList $ map instr2SExpr instrs0

instr2SExpr (W.Atomic instr) = atomicInstr2SExpr instr

atomicInstr2SExpr :: W.AtomicInstr -> WellFormedSExpr Atom
atomicInstr2SExpr (W.GetLocal n) =
    WFSList [WFSAtom GetLocal, WFSAtom (Num $ fromIntegral n)]
atomicInstr2SExpr (W.ConstI W.I32 n) =
    WFSList [WFSAtom ConstI32, WFSAtom (Num n)]
atomicInstr2SExpr W.Return = WFSAtom Return

type2SExpr W.I32 = WFSAtom I32

-----------------
-- SExpr atoms --
-----------------

data Atom = Module
          | Func
          | Export
          | Param
          | Result
          | I32
          | GetLocal
          | ConstI32
          | Return
          | Num Integer
          | Str String

instance Show Atom where
    show Module = "module"
    show Func = "func"
    show Export = "export"
    show Param = "param"
    show Result = "result"
    show I32 = "i32"
    show GetLocal = "get-local"
    show ConstI32 = "i32.const"
    show Return = "return"
    show (Num n) = show n
    show (Str s) = "\"" ++ s ++ "\""

--------------------
-- SExpr printing --
--------------------

-- We might do this lazilly except SExprPrinter constrains the Text
-- implementation.
sexpr2Text = encodeOne printer . fromWellFormed

printer :: SExprPrinter Atom (SExpr Atom)
printer = basicPrint atomPrint
    where atomPrint = t . show

--------------------
-- Text utilities --
--------------------

t :: String -> Text.Text
t = Text.pack

