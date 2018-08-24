
module Codegen (codegen, build_locals) where

import Control.Monad (liftM, foldM)
import Control.Monad.Trans.State

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
codegen_func func_syms (Ast.Func name0 vis arg_names body0) =
    do arg_syms <- foldM setup_arg S.init arg_names
       let (body, locals_syms) = runState (build_locals body0) arg_syms
       body_instrs0 <- codegen_expr (CGInfo func_syms locals_syms) body
       let body_instrs = body_instrs0 ++ [W.Atomic W.Return]
       let arg_types = take (S.num arg_syms) i32s
       let local_types = take ((S.num locals_syms) - (S.num arg_syms)) i32s
       return $ W.Func name W.I32 arg_types local_types body_instrs
    where setup_arg arg_syms arg =
            case S.new arg_syms arg of
                Just s -> return s
                Nothing -> fail $ "Duplicate function argument '" ++ arg ++ "'"
          i32s = repeat W.I32
          name = case vis of
            Ast.Public -> Just name0
            Ast.Private -> Nothing

-- Prepare locals
-----------------

build_locals :: Ast.Expr -> State (S.Symtab String) Ast.Expr
build_locals (Ast.BOp op left0 right0) =
    do left <- build_locals left0
       right <- build_locals right0
       return $ Ast.BOp op left right
build_locals (Ast.Let var0 let0 in0) =
    do do_rename <- s_member var0
       (var, in2) <- if do_rename
                        then do v <- s_new_var
                                let in1 = rename_expr var0 v in0
                                return (v, in1)
                        else do s_new var0
                                return (var0, in0)
       let_ <- build_locals let0
       in_ <- build_locals in2
       return (Ast.Let var let_ in_)
build_locals (Ast.Call callee args0) =
    do args <- mapM build_locals args0
       return $ Ast.Call callee args
build_locals e@(Ast.Var _) = return e
build_locals e@(Ast.Lit32 _) = return e

s_member :: String -> State (S.Symtab String) Bool
s_member var = do s <- get
                  return $ S.member s var

s_new var = do s <- get
               put $ case S.new s var of
                        Just x -> x
                        Nothing -> error "Variable cannot be not-unique here"
               return ()

s_new_var = do s0 <- get
               let (v, s) = S.new_var new_sym s0
               put s
               return v
    where new_sym symid = "V_" ++ (show symid)

rename_expr old new (Ast.BOp op left0 right0) =
    Ast.BOp op (rename_expr old new left0) (rename_expr old new right0)
rename_expr old new (Ast.Let letvar let0 in0) =
    let let_ = if old == letvar
                then let0
                else rename_expr old new let0
        in_ = rename_expr old new in0
    in Ast.Let letvar let_ in_
rename_expr old new (Ast.Call callee args) =
    Ast.Call callee (map (rename_expr old new) args)
rename_expr old new (Ast.Var var) =
    Ast.Var $ if var == old then new else var
rename_expr _ _ (Ast.Lit32 n) = Ast.Lit32 n

-- Generate code
----------------

codegen_expr :: CGInfo -> Ast.Expr -> Either String [W.Instr]
codegen_expr info (Ast.BOp op left right) =
    do leftcode <- codegen_expr info left
       rightcode <- codegen_expr info right
       return $ leftcode ++ rightcode ++ opcode
    where opcode = [W.Atomic ((case op of
                                Ast.Add -> W.Add
                                Ast.Subtract -> W.Sub
                                Ast.Multiply -> W.Mul
                                Ast.Divide -> W.DivS
                                -- Note that without a type system we must
                                -- use I32 since that's what the relational
                                -- instructions return.
                                Ast.LessThan -> W.LtS
                                Ast.LessThanEqualTo -> W.LeS
                                Ast.GreaterThan -> W.GtS
                                Ast.GreaterThanEqualTo -> W.GeS
                                Ast.NotEqual -> W.Ne
                                Ast.Equal -> W.Eq)
                              W.I32)]

codegen_expr info (Ast.Let var in_expr let_expr) =
    do in_code <- codegen_expr info in_expr
       set_code <- case S.lookup (cgi_locals info) var of
           Just local_id -> return $ W.Atomic $ W.SetLocal local_id
           Nothing -> Left $ "No such local variable '" ++ var ++ "'"
       let_code <- codegen_expr info let_expr
       return $ in_code ++ [set_code] ++ let_code

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

