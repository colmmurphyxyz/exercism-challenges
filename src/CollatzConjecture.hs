module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n 
    | n <= 0 = Nothing
    | otherwise =
        collatz' 0 n where
        collatz' acc 1 = Just acc
        collatz' acc m
            | even m = collatz' (acc + 1) (m `div` 2)
            | otherwise = collatz' (acc + 1) (3 * m + 1)

main :: IO ()
main = do
    print (collatz 12)
    print (collatz 4)
    print (collatz 10000001)
