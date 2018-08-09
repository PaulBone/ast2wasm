
module Codegen (codegen) where

import qualified Ast
import qualified Wasm as W
import qualified Data.Map.Strict as Map

codegen :: Ast.Module -> Either String W.Module
codegen (Ast.Module funcs0) = do funcs <- mapM codegen_func funcs0
                                 return $ W.Module funcs

codegen_func (Ast.Func name0 vis arg_names body) =
    do info <- foldl setup_arg (return cginfo_init) arg_names
       body_instrs0 <- codegen_expr body info
       let body_instrs = body_instrs0 ++ [W.Atomic W.Return]
       return $ W.Func name W.I32 arg_types body_instrs
    where setup_arg (Left e) _ = Left e 
          setup_arg (Right info) arg = cginfo_add_local info arg
          arg_types = take (length arg_names) (repeat W.I32)
          name = case vis of
            Ast.Public -> Just name0
            Ast.Private -> Nothing

codegen_expr :: Ast.Expr -> CGInfo -> Either String [W.Instr]
codegen_expr (Ast.BOp op left right) info =
    do leftcode <- codegen_expr left info
       rightcode <- codegen_expr right info
       return $ leftcode ++ rightcode ++ opcode
    where opcode = [W.Atomic ((case op of
                                Ast.Add -> W.Add
                                Ast.Subtract -> W.Sub
                                Ast.Multiply -> W.Mul
                                Ast.Divide -> W.DivS) W.I32)]

codegen_expr (Ast.Var var) info =
    case lookup_local info var of
        Just num -> return $ [W.Atomic $ W.GetLocal num]
        Nothing -> Left $ "No such variable: " ++ var

codegen_expr (Ast.Lit32 n) _ = return $ [W.Atomic $ W.ConstI W.I32 n]

---------------------------------------------------------------------------

data CGInfo = CGInfo {
        cgi_locals      :: Map.Map String Int,
        _cgi_next_local :: Int
    }

cginfo_init = CGInfo Map.empty 0

lookup_local info var = Map.lookup var (cgi_locals info)

cginfo_add_local (CGInfo locals next) var =
     if Map.member var locals
        then Left $ "Duplicate variable " ++ var 
        else return $ CGInfo (Map.insert var next locals) (next + 1)

