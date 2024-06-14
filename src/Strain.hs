module Strain (keep, discard) where

-- Return a new collection containing only the elements that do not match the predicate
-- i.e. discard all matching elements
discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)

-- Return a new collection containing only elements that match the predicate
-- i.e. keep all matching elements
-- i.e the filter function
keep :: (a -> Bool) -> [a] -> [a]
keep p [] = []
keep p (x:xs)
    | p x = x : keep p xs
    | otherwise = keep p xs