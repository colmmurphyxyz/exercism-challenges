module Anagram (anagramsFor) where
import Data.Char (toUpper)
import Data.MultiSet (fromList)

-- returns True if the two strings given are anagrams of each other
isAnagram :: String -> String -> Bool
isAnagram xs xss
    | map toUpper xs == map toUpper xss = False -- Strings are not anagrams if they are equal
    | otherwise =
        let xs' = map toUpper xs
            xss' = map toUpper xss in
        fromList xs' == fromList xss'

-- Return all elements of xss that are anagrams of xs
-- Case sensitive btw
anagramsFor :: String -> [String] -> [String]
anagramsFor [] _ = []
anagramsFor _ [] = []
anagramsFor target candidates =
    filter (isAnagram target) candidates
