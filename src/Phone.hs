module Phone (number) where

validatePhoneNumber :: String -> Maybe String
validatePhoneNumber xs
    | length xs /= 10 = Nothing
    | head xs `elem` ['0', '1'] = Nothing
    | xs !! 3 `elem` ['0', '1'] = Nothing
    | otherwise = Just xs

-- accepts a list of Digit characters, and removes the leading area code, if any
removeCountryCode :: String -> String
removeCountryCode xs
    | length xs == 11 && head xs == '1' = tail xs
    | otherwise = xs

removeNonDigits :: String -> String
removeNonDigits = filter (\x -> x `elem` ['0'..'9'])

number :: String -> Maybe String
number xs = do
    let cleaned = removeNonDigits xs
    let noAreaCode = removeCountryCode cleaned
    validatePhoneNumber noAreaCode
