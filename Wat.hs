
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

func2SExpr (W.Func name0 rettype0 params0 locals0 instrs0) =
    WFSList $ [WFSAtom Func] ++ name ++ params ++ [rettype] ++ locals ++
        instrs
    where name = case name0 of
            Nothing -> []
            Just n -> [WFSList [WFSAtom Export, WFSAtom (Str n)]]
          params = typeList Param params0
          locals = typeList Local locals0
          typeList atom l = case l of
                            _:_ -> [WFSList $ (WFSAtom atom) : map type2SExpr l]
                            [] -> []
          rettype = WFSList [WFSAtom Result, type2SExpr rettype0]
          instrs = map instr2SExpr instrs0

instr2SExpr (W.Atomic instr) = atomicInstr2SExpr instr
instr2SExpr (W.If types0 true0 false0) =
        WFSList [WFSAtom If, types, true, false]
    where types = makeList Result (map type2SExpr types0)
          true = makeInstrList Then true0
          false = makeInstrList Else false0

atomicInstr2SExpr :: W.AtomicInstr -> WellFormedSExpr Atom
atomicInstr2SExpr (W.Add W.I32) = WFSAtom AddI32
atomicInstr2SExpr (W.Sub W.I32) = WFSAtom SubI32
atomicInstr2SExpr (W.Mul W.I32) = WFSAtom MulI32
atomicInstr2SExpr (W.DivS W.I32) = WFSAtom DivSI32
atomicInstr2SExpr (W.DivU W.I32) = WFSAtom DivUI32
atomicInstr2SExpr (W.RemS W.I32) = WFSAtom RemSI32
atomicInstr2SExpr (W.RemU W.I32) = WFSAtom RemUI32
atomicInstr2SExpr (W.LtS W.I32) = WFSAtom LtSI32
atomicInstr2SExpr (W.LtU W.I32) = WFSAtom LtUI32
atomicInstr2SExpr (W.LeS W.I32) = WFSAtom LeSI32
atomicInstr2SExpr (W.LeU W.I32) = WFSAtom LeUI32
atomicInstr2SExpr (W.GtS W.I32) = WFSAtom GtSI32
atomicInstr2SExpr (W.GtU W.I32) = WFSAtom GtUI32
atomicInstr2SExpr (W.GeS W.I32) = WFSAtom GeSI32
atomicInstr2SExpr (W.GeU W.I32) = WFSAtom GeUI32
atomicInstr2SExpr (W.Ne W.I32) = WFSAtom NeI32
atomicInstr2SExpr (W.Eq W.I32) = WFSAtom EqI32
atomicInstr2SExpr (W.Call n) =
    WFSList [WFSAtom Call, WFSAtom (Num $ fromIntegral n)]
atomicInstr2SExpr (W.GetLocal n) =
    WFSList [WFSAtom GetLocal, WFSAtom (Num $ fromIntegral n)]
atomicInstr2SExpr (W.SetLocal n) =
    WFSList [WFSAtom SetLocal, WFSAtom (Num $ fromIntegral n)]
atomicInstr2SExpr (W.ConstI W.I32 n) =
    WFSList [WFSAtom ConstI32, WFSAtom (Num n)]
atomicInstr2SExpr W.Return = WFSAtom Return

type2SExpr W.I32 = WFSAtom I32

makeList a l = WFSList ((WFSAtom a):l)

makeInstrList a l = makeList a (map instr2SExpr l)

-----------------
-- SExpr atoms --
-----------------

data Atom = Module
          | Func
          | Export
          | Param
          | Local
          | Result
          | If
          | Then
          | Else
          | I32
          | AddI32
          | SubI32
          | MulI32
          | DivSI32
          | DivUI32
          | RemSI32
          | RemUI32
          | LtSI32
          | LtUI32
          | LeSI32
          | LeUI32
          | GtSI32
          | GtUI32
          | GeSI32
          | GeUI32
          | NeI32
          | EqI32
          | Call
          | GetLocal
          | SetLocal
          | ConstI32
          | Return
          | Num Integer
          | Str String

instance Show Atom where
    show Module = "module"
    show Func = "func"
    show Export = "export"
    show Param = "param"
    show Local = "local"
    show Result = "result"
    show If = "if"
    show Then = "then"
    show Else = "else"
    show I32 = "i32"
    show AddI32 = "i32.add"
    show SubI32 = "i32.sub"
    show MulI32 = "i32.mul"
    show DivSI32 = "i32.div_s"
    show DivUI32 = "i32.div_u"
    show RemSI32 = "i32.rem_s"
    show RemUI32 = "i32.rem_u"
    show LtSI32 = "i32.lt_s"
    show LtUI32 = "i32.lt_u"
    show LeSI32 = "i32.le_s"
    show LeUI32 = "i32.le_u"
    show GtSI32 = "i32.gt_s"
    show GtUI32 = "i32.gt_u"
    show GeSI32 = "i32.ge_s"
    show GeUI32 = "i32.ge_u"
    show NeI32 = "i32.ne"
    show EqI32 = "i32.eq"
    show Call = "call"
    show GetLocal = "get_local"
    show SetLocal = "set_local"
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

