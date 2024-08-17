module Luhn (isValid) where

import Data.Char(digitToInt)

filterOutSpaces :: String -> String
filterOutSpaces = filter (/=' ')

mapOnlyEvenNumbered f []     = []
mapOnlyEvenNumbered f (x:xs) = x : mapOnlyOddNumbered f xs

mapOnlyOddNumbered f []      = []
mapOnlyOddNumbered f (x:xs)  = f x : mapOnlyEvenNumbered f xs

luhnDouble :: Int -> Int
luhnDouble n
    | n >= 5 = (2 * n) - 9
    | otherwise = 2 * n

isValid :: String -> Bool
isValid xs
    | any (`notElem` "0123456789 ") xs = False
    | otherwise = do
        let formatted = map digitToInt $ filterOutSpaces xs
        isValid' formatted

isValid' :: [Int] -> Bool
isValid' xs
    | length xs <= 1 = False
    | otherwise = do
        let doubled = mapOnlyEvenNumbered luhnDouble (reverse xs)
        (sum doubled) `mod` 10 == 0


main :: IO ()
main = do
    print $ isValid "4539 3195 0343 6467" -- True
    print $ isValid "8273 1232 7352 0569" -- False
    print $ isValid "091" -- True
    print $ isValid "59" -- True
    print $ isValid "095 245 88" -- True
    print $ isValid "234 567 891 234" -- True
    print $ isValid "1 2345 6789 1234 5678 9012" -- False

