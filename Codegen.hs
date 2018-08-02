
module Codegen (codegen) where

import Ast
import Wasm
import qualified Data.Map.Strict as Map

data CGInfo = CGInfo {
        locals      :: Map.Map String Int
    }

codegen :: Ast.Func -> Maybe Wasm.Func
codegen = codegen_func

codegen_func (Ast.Func name vis arg_names body) =
    do (locals, _) <- foldl setup_arg (Just (Map.empty, 0)) arg_names
       body_instrs <- codegen_expr body (CGInfo locals)
       return $ Wasm.Func I32 arg_types body_instrs
    where setup_arg Nothing _ = Nothing
          setup_arg (Just (map, next)) arg =
             if Map.member arg map then Nothing
                                   else Just (Map.insert arg next map,
                                              next + 1)
          arg_types = take (length arg_names) (repeat I32)

codegen_expr (Var var) info = do num <- lookup_local info var
                                 return $ Atomic $ GetLocal num

lookup_local (CGInfo locals) var = Map.lookup var locals

