
module Check (check) where

import Ast

--
-- Here's where you'd add type checking and other semantic checks
--
check :: Module -> Either String ()
check _ = okay

okay = return ()
