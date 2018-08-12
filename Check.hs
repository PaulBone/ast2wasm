
module Check (check) where

import Data.List.Unique (repeated)

import Ast

check :: Module -> Either String ()
check (Module funcs) =
    do let funcNames = map name funcs
       case repeated funcNames of
            (repName:_) -> error $ 
                "Duplicate function name '" ++ repName ++ "'"
            [] -> okay
       _ <- mapM checkFunc funcs
       okay

checkFunc :: Func -> Either String ()
checkFunc func = case repeated (args func) of
                    (repName:_) -> error $
                        "Duplicate function parameter: '" ++ repName ++ "'"
                    [] -> okay

okay = return ()
