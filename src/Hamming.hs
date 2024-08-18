module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | any (`notElem` "CAGT") (xs ++ ys) = Nothing
    | otherwise = Just $ distance' xs ys

distance' :: String -> String -> Int
distance' xs ys =
    length $ filter (\a -> fst a /= snd a) (zip xs ys) 

main :: IO ()
main = do
    print $ distance "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT"
    print $ distance "" ""
