module Darts (score) where

score :: Float -> Float -> Int
score x y =
    let distance = sqrt (x ** 2 + y ** 2)
    in case distance of
        _ | distance <= 1 -> 10
        _ | distance <= 5 -> 5
        _ | distance <= 10 -> 1
        _ -> 0

main :: IO ()
main = do
    print (score 0.8 0.8)
    print (score 2.5 2.5)
    print (score 5.0 5.0)
    print (score 10.0 10.0)

