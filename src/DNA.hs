module DNA (
    toRNA,
    nucleotideCounts, Nucleotide(..)
) where

import Data.Map (Map, fromListWith)

toRNA :: String -> Either Char String
toRNA dna = toRNA' dna [] where
    toRNA' [] rna = Right rna
    toRNA' dna rna
        | head dna == 'G' = toRNA' (tail dna) (rna ++ ['C'])
        | head dna == 'C' = toRNA' (tail dna) (rna ++ ['G'])
        | head dna == 'T' = toRNA' (tail dna) (rna ++ ['A'])
        | head dna == 'A' = toRNA' (tail dna) (rna ++ ['U'])
        | otherwise = Left $ head dna


data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

isValidNucleotide :: Char -> Bool
isValidNucleotide = (`elem` "ACGT")

toNucleotide :: Char -> Nucleotide
toNucleotide x
    | x == 'A' = A
    | x == 'C' = C
    | x == 'G' = G
    | x == 'T' = T

-- Pairs each 
pairWithOne :: String -> [(Nucleotide, Int)]
pairWithOne = map (\x -> (toNucleotide x, 1))

sumPairs :: [(Nucleotide, Int)] -> Map Nucleotide Int
sumPairs = fromListWith (+)

sumNucleotides :: String -> Map Nucleotide Int
sumNucleotides xs = sumPairs $ pairWithOne xs

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = if not (all isValidNucleotide xs)
    then Left "Error"
    else Right $ sumNucleotides xs