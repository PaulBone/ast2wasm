
module Symtab (Symtab,
               Symtab.init,
               Symtab.lookup,
               new,
               Symtab.member,
               new_var,
               num) where

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

new_var :: (Ord a, Show a) => (Int -> a) -> Symtab a -> (a, Symtab a)
new_var new_sym s =
    let symid = next_id s
        sym = new_sym symid
        newtab = if M.member sym (symbols s)
                    then error ("Duplicate symbol in new_var '" ++
                            (show sym) ++ "'")
                    else M.insert sym symid (symbols s)
    in (sym, Symtab newtab (symid + 1))

member :: Ord a => Symtab a -> a -> Bool
member s x = M.member x (symbols s)

num :: Symtab a -> Int
num s = next_id s

