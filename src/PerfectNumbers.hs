module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factorsOf :: Int -> [Int]
factorsOf n = [x | x <- [1..n `div` 2], n `mod` x == 0]

aliquotSum :: Int -> Int
aliquotSum = sum . factorsOf

classify :: Int -> Maybe Classification
classify n = case compare n 0 of
    LT -> Nothing
    EQ -> Nothing
    GT -> case compare (aliquotSum n) n of
        LT -> Just Deficient
        EQ -> Just Perfect
        GT -> Just Abundant

