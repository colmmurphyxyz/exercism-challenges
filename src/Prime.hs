module Prime (nth) where

primes :: [Integer]
primes = 2: 3: calcNextPrimes (tail primes) [5, 7 .. ]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, _:biggerSquareP) = span (< p * p) candidates in
      smallerSquareP ++ calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]

nth :: Int -> Maybe Integer
nth n
    | n <= 0 = Nothing
    | otherwise = Just $ primes !! (n - 1)
    
