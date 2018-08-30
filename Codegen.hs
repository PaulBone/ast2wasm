
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
       let (body, locals_syms0) = runState (build_locals body0) arg_syms
       (body_instrs0, info) <- runStateT (codegen_expr body)
                                      (CGInfo func_syms locals_syms0)
       let locals_syms = cgi_locals info
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
build_locals (Ast.Case switch_expr0 cases0) =
    do switch_expr <- build_locals switch_expr0
       cases <- mapM build_case cases0
       return $ Ast.Case switch_expr cases
    where build_case (Ast.PatExpr pat expr0) =
            (build_locals expr0 >>= (return . (Ast.PatExpr pat)))
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

rename_expr old new (Ast.BOp op left0 right0) =
    Ast.BOp op (rename_expr old new left0) (rename_expr old new right0)
rename_expr old new (Ast.Let letvar let0 in0) = Ast.Let letvar let_ in_
    where let_ = if old == letvar
                    then let0
                    else rename_expr old new let0
          in_ = rename_expr old new in0
rename_expr old new (Ast.Case expr0 cases0) = Ast.Case expr cases
    where expr = rename_expr old new expr0
          cases = map rename_case cases0
          rename_case (Ast.PatExpr p pexpr) =
            Ast.PatExpr p (rename_expr old new pexpr)
rename_expr old new (Ast.Call callee args) =
    Ast.Call callee (map (rename_expr old new) args)
rename_expr old new (Ast.Var var) =
    Ast.Var $ if var == old then new else var
rename_expr _ _ (Ast.Lit32 n) = Ast.Lit32 n

-- Generate code
----------------

codegen_expr :: Ast.Expr ->
    StateT CGInfo (Either String) [W.Instr]
codegen_expr (Ast.BOp op left right) =
    do leftcode <- codegen_expr left
       rightcode <- codegen_expr right
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

codegen_expr (Ast.Let var in_expr let_expr) =
    do in_code <- codegen_expr in_expr
       info <- get
       set_code <- case S.lookup (cgi_locals info) var of
           Just local_id -> return $ W.Atomic $ W.SetLocal local_id
           Nothing -> fail $ "No such local variable '" ++ var ++ "'"
       let_code <- codegen_expr let_expr
       return $ in_code ++ [set_code] ++ let_code

codegen_expr (Ast.Case expr cases) =
    do exprcode <- codegen_expr expr
       info0 <- get
       let (varname, locals) = S.new_var new_sym (cgi_locals info0)
       let var = maybe (error "Fresh var not found") id $
                    S.lookup locals varname
       let setcode = [W.Atomic $ W.SetLocal var]
       let info = info0 { cgi_locals = locals }
       put info
       casecodes <- mapM (codegen_case var) cases
       let casescode = make_if_chain casecodes
       return $ exprcode ++ setcode ++ casescode

codegen_expr (Ast.Call callee args) =
    do argscode <- liftM concat $ mapM codegen_expr args
       info <- get
       callcode <- case S.lookup (cgi_funcs info) callee of
           Just num -> return $ [W.Atomic $ W.Call num]
           Nothing -> fail $ "No such function name '" ++ callee ++ "'"
       return $ argscode ++ callcode

codegen_expr (Ast.Var var) =
    do info <- get
       case S.lookup (cgi_locals info) var of
            Just num -> return $ [W.Atomic $ W.GetLocal num]
            Nothing -> fail $ "No such variable: " ++ var

codegen_expr (Ast.Lit32 n) = return $ [W.Atomic $ W.ConstI W.I32 n]

data CaseCode = CaseCode
                    -- Instructions to prepare and test for the condition,
                    -- laving an I32 on the top-of-stack.  Nothing if the
                    -- instructions are always executed
                    (Maybe [W.Instr])
                    -- True block (code to execute if true).
                    [W.Instr]

codegen_case val (Ast.PatExpr pat expr) =
    do block_instrs <- codegen_expr expr
       return $ CaseCode (make_prep_test_instrs pat) block_instrs
    where make_prep_test_instrs (Ast.Number n) = Just $
            map W.Atomic [W.GetLocal val, W.ConstI W.I32 n, W.Eq W.I32]
          make_prep_test_instrs Ast.Wildcard = Nothing

make_if_chain :: [CaseCode] -> [W.Instr]
make_if_chain [] = [W.Unreachable]
make_if_chain ((CaseCode (Just test) instrs):xs) =
    test ++ [W.If [W.I32] instrs elsecode]
    where elsecode = make_if_chain xs
make_if_chain ((CaseCode Nothing instrs):_) = instrs

---------------------------------------------------------------------------

data CGInfo = CGInfo {
        cgi_funcs       :: S.Symtab String,
        cgi_locals      :: S.Symtab String
    }

new_sym symid = "V_" ++ (show symid)

