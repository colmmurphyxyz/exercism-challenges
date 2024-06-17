module ReverseString (reverseString) where

reverseString :: String -> String
reverseString str
    | null str = ""
    | otherwise = reverseString (tail str) ++ [head str]

main :: IO ()
main = print (reverseString "Hello, World!")