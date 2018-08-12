
module Symtab (Symtab, Symtab.init, Symtab.lookup, new) where

import qualified Data.Map.Strict as M

data Symtab a = Symtab {
        symbols     :: M.Map a Int,
        next_id     :: Int
    }

init :: Symtab a
init = Symtab M.empty 0

lookup :: Ord a => Symtab a -> a -> Maybe Int
lookup s x = M.lookup x (symbols s)

new :: Ord a => Symtab a -> a -> Maybe (Symtab a)
new s x = if not $ M.member x (symbols s)
            then let symid = (next_id s)
                     newtab = M.insert x symid (symbols s)
                 in Just (Symtab newtab (symid + 1))
            else Nothing

