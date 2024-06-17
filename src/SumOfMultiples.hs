module SumOfMultiples (sumOfMultiples) where

rangeUpTo :: Integer -> [Integer]
rangeUpTo n
    | n <= 0 = []
    | n == 1 = [1]
    | otherwise = rangeUpTo' [] 1 where
        rangeUpTo' acc x
            | x <= 0 = []
            | x == n = acc ++ [x]
            | otherwise = rangeUpTo' (acc ++ [x]) (x + 1)

firstNMultiples :: Integer -> Integer -> [Integer]
firstNMultiples n x = map (* x) (rangeUpTo n)

union :: [Integer] -> [Integer] -> [Integer]
union xs ys = xs ++ filter (`notElem` xs) ys

unionAll :: [[Integer]] -> [Integer]
unionAll = foldl union []

getThingy :: Integer -> Integer -> Integer
getThingy x y
    | y == 0 = 0
    | x `mod` y == 0 = (x `div` y) - 1
    | otherwise = x `div` y

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit
    | null factors = 0
    | otherwise = sum $ foldl union [] $ map (\x -> firstNMultiples (getThingy limit x) x) factors

main :: IO ()
main = do
    print (sumOfMultiples [7, 13, 17] 20) -- 51
    print (sumOfMultiples [4, 6] 15) --30
    print (sumOfMultiples [5, 6, 8] 150) -- 4419
    print (sumOfMultiples [43, 47] 10000) -- 2203160
    print (sumOfMultiples [1] 100) -- 4950
    print (sumOfMultiples [0] 1) -- 0
    print (sumOfMultiples [3, 0] 4) --3
