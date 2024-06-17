module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock {
    hours :: Int,
    minutes :: Int
}

-- implement an equality function for the Clock data type
instance Show Clock where
    show = toString

instance Eq Clock where
    (==) a b = hours a == hours b && minutes a == minutes b

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = 
    let carriedHours = m `div` 60
    in Clock ((h + carriedHours) `mod` 24) (m `mod` 60)

showPadded :: Int -> String
showPadded n
    | n < 10 = "0" ++ show n
    | otherwise = show n

toString :: Clock -> String
toString clock = showPadded (hours clock) ++ ":" ++ showPadded (minutes clock)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min clock = 
    let sumMinutes = minutes clock + min
        carriedHours = sumMinutes `div` 60
        sumHours = hours clock + hour + carriedHours
    in fromHourMin (sumHours `mod` 24) (sumMinutes `mod` 60)
