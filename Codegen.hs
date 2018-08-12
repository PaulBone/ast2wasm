
module Codegen (codegen) where

import Control.Monad (liftM, foldM)

import qualified Ast
import qualified Wasm as W
import qualified Symtab as S

codegen :: Ast.Module -> Either String W.Module
codegen (Ast.Module funcs0) =
    do func_syms <- foldM make_func_sym S.init funcs0
       funcs <- mapM (codegen_func func_syms) funcs0
       return $ W.Module funcs
    where make_func_sym symtab func =
            let name = Ast.name func
            in case S.new symtab name of
                  Just x -> return x
                  Nothing -> fail $ "Duplicate function name '" ++ name ++ "'"

codegen_func :: S.Symtab String -> Ast.Func -> Either String W.Func
codegen_func func_syms (Ast.Func name0 vis arg_names body) =
    do arg_syms <- foldM setup_arg S.init arg_names
       body_instrs0 <- codegen_expr (CGInfo func_syms arg_syms) body
       let body_instrs = body_instrs0 ++ [W.Atomic W.Return]
       return $ W.Func name W.I32 arg_types body_instrs
    where setup_arg arg_syms arg =
            case S.new arg_syms arg of
                Just s -> return s
                Nothing -> fail $ "Duplicate function argument '" ++ arg ++ "'"
          arg_types = take (length arg_names) (repeat W.I32)
          name = case vis of
            Ast.Public -> Just name0
            Ast.Private -> Nothing

codegen_expr :: CGInfo -> Ast.Expr -> Either String [W.Instr]
codegen_expr info (Ast.BOp op left right) =
    do leftcode <- codegen_expr info left
       rightcode <- codegen_expr info right
       return $ leftcode ++ rightcode ++ opcode
    where opcode = [W.Atomic ((case op of
                                Ast.Add -> W.Add
                                Ast.Subtract -> W.Sub
                                Ast.Multiply -> W.Mul
                                Ast.Divide -> W.DivS) W.I32)]

codegen_expr info (Ast.Call callee args) =
    do argscode <- liftM concat $ mapM (codegen_expr info) args
       callcode <- case S.lookup (cgi_funcs info) callee of
           Just num -> return $ [W.Atomic $ W.Call num]
           Nothing -> Left $ "No such function name '" ++ callee ++ "'"
       return $ argscode ++ callcode

codegen_expr info (Ast.Var var) =
    case S.lookup (cgi_locals info) var of
        Just num -> return $ [W.Atomic $ W.GetLocal num]
        Nothing -> Left $ "No such variable: " ++ var

codegen_expr _ (Ast.Lit32 n) = return $ [W.Atomic $ W.ConstI W.I32 n]

---------------------------------------------------------------------------

data CGInfo = CGInfo {
        cgi_funcs       :: S.Symtab String,
        cgi_locals      :: S.Symtab String
    }

