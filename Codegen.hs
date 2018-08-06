
module Codegen (codegen) where

import qualified Ast
import qualified Wasm as W
import qualified Data.Map.Strict as Map

codegen :: Ast.Module -> Maybe W.Module
codegen (Ast.Module funcs0) = do funcs <- mapM codegen_func funcs0
                                 return $ W.Module funcs

codegen_func (Ast.Func name0 vis arg_names body) =
    do info <- foldl setup_arg (Just cginfo_init) arg_names
       body_instrs0 <- codegen_expr body info
       let body_instrs = body_instrs0 ++ [W.Atomic W.Return]
       return $ W.Func name W.I32 arg_types body_instrs
    where setup_arg Nothing _ = Nothing
          setup_arg (Just info) arg = cginfo_add_local info arg
          arg_types = take (length arg_names) (repeat W.I32)
          name = case vis of
            Ast.Public -> Just name0
            Ast.Private -> Nothing

codegen_expr (Ast.Var var) info = do num <- lookup_local info var
                                     return $ [W.Atomic $ W.GetLocal num]
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
        then Nothing
        else Just $ CGInfo (Map.insert var next locals) (next + 1)

