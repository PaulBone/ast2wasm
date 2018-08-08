
module Wat (writeWat) where

import qualified Data.Text as Text
import Data.SCargot.Print
import Data.SCargot.Repr

import qualified Wasm as W

writeWat :: W.Module -> String
writeWat = Text.unpack . sexpr2Text .  wasm2SExpr

----------------------------
-- Convert Wasm to SExprs --
----------------------------

wasm2SExpr (W.Module funcs0) = WFSList ([WFSAtom Module] ++ funcs)
    where funcs = map func2SExpr funcs0

func2SExpr (W.Func name0 rettype0 params0 instrs0) =
    WFSList $ [WFSAtom Func] ++ name ++ params ++ [rettype] ++ instrs
    where name = case name0 of
            Nothing -> []
            Just n -> [WFSList [WFSAtom Export, WFSAtom (Str n)]]
          params = map (\p -> WFSList [WFSAtom Param, type2SExpr p])
                       params0
          rettype = WFSList [WFSAtom Result, type2SExpr rettype0]
          instrs = map instr2SExpr instrs0

instr2SExpr (W.Atomic instr) = atomicInstr2SExpr instr

atomicInstr2SExpr :: W.AtomicInstr -> WellFormedSExpr Atom
atomicInstr2SExpr (W.Add W.I32) = WFSAtom AddI32
atomicInstr2SExpr (W.Sub W.I32) = WFSAtom SubI32
atomicInstr2SExpr (W.Mul W.I32) = WFSAtom MulI32
atomicInstr2SExpr (W.DivS W.I32) = WFSAtom DivSI32
atomicInstr2SExpr (W.DivU W.I32) = WFSAtom DivUI32
atomicInstr2SExpr (W.RemS W.I32) = WFSAtom RemSI32
atomicInstr2SExpr (W.RemU W.I32) = WFSAtom RemUI32
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
          | AddI32
          | SubI32
          | MulI32
          | DivSI32
          | DivUI32
          | RemSI32
          | RemUI32
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
    show AddI32 = "i32.add"
    show SubI32 = "i32.sub"
    show MulI32 = "i32.mul"
    show DivSI32 = "i32.div_s"
    show DivUI32 = "i32.div_u"
    show RemSI32 = "i32.rem_s"
    show RemUI32 = "i32.rem_u"
    show GetLocal = "get_local"
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

