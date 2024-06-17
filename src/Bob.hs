module Bob (responseFor) where

import Data.Char (isSpace)

isQuestion :: [Char] -> Bool
isQuestion str =
    let filtered = filter (not . isSpace) str in
        filtered /= "" && last filtered == '?'

isLower :: Char -> Bool
isLower = (`elem` ['a'..'z'])

isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])

isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c

isAllCaps :: [Char] -> Bool
isAllCaps str =
    let filtered = filter isAlpha str in
        filtered /= "" && all (`elem` ['A'..'Z']) filtered

responseFor :: String -> String
responseFor xs
    | str == "" = "Fine. Be that way!"
    | isAllCaps str && isQuestion str = "Calm down, I know what I'm doing!"
    | isQuestion str = "Sure."
    | isAllCaps str = "Whoa, chill out!"
    | otherwise = "Whatever."
    where str = filter (not . isSpace) xs