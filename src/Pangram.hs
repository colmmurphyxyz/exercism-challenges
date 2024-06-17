module Pangram (isPangram) where

import Data.Char (toLower)

allLower :: [Char] -> [Char]
allLower = map toLower

alphabet :: [Char]
alphabet = ['a'..'z']

isPangram :: String -> Bool
isPangram text =
    all (`elem` allLower text) alphabet