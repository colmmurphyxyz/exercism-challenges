module Acronym (abbreviate) where
import Data.Char (toUpper)

replaceHyphens :: String -> String
replaceHyphens [] = []
replaceHyphens (x:xs)
    | x == '-' = ' ' : replaceHyphens xs
    | otherwise = x : replaceHyphens xs

filterOutPunctuation :: String -> String
filterOutPunctuation = filter (`notElem` ",.!?_")

isUpper :: Char -> Bool
isUpper c = c `elem` ['A'..'Z']

isLower :: Char -> Bool
isLower c = c `elem` ['a'..'z']

separatePascalCaseWords :: String -> String
separatePascalCaseWords [] = []
separatePascalCaseWords (x:xs)
    | null xs = [x]
    | isLower x && isUpper (head xs) = x : ' ' : separatePascalCaseWords xs
    | otherwise = x : separatePascalCaseWords xs


abbreviate :: String -> String
abbreviate xs = map (toUpper . head) $ words $ separatePascalCaseWords $ replaceHyphens $ filterOutPunctuation xs

main :: IO ()
main = do
    print $ abbreviate "As Soon As Possible"
    print $ abbreviate "HyperText Markup Language"
    print $ abbreviate "Ruby on Rails"
    print $ abbreviate "Complementary metal-oxide semiconductor"