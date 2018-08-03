
module Codegen (codegen) where

import Ast
import Wasm
import qualified Data.Map.Strict as Map

codegen :: Ast.Func -> Maybe Wasm.Func
codegen = codegen_func

codegen_func (Ast.Func _ _ arg_names body) =
    do info <- foldl setup_arg (Just cginfo_init) arg_names
       body_instrs <- codegen_expr body info
       return $ Wasm.Func I32 arg_types body_instrs
    where setup_arg Nothing _ = Nothing
          setup_arg (Just info) arg = cginfo_add_local info arg
          arg_types = take (length arg_names) (repeat I32)

codegen_expr (Var var) info = do num <- lookup_local info var
                                 return $ Atomic $ GetLocal num

---------------------------------------------------------------------------

data CGInfo = CGInfo {
        cgi_locals      :: Map.Map String Int,
        cgi_next_local  :: Int
    }

cginfo_init = CGInfo Map.empty 0

lookup_local info var = Map.lookup var (cgi_locals info)

cginfo_add_local (CGInfo locals next) var =
     if Map.member var locals
        then Nothing
        else Just $ CGInfo (Map.insert var next locals) (next + 1)

