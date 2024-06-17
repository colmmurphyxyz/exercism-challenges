module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (earthYearInSeconds * planetYearRatio planet)
    where
        earthYearInSeconds = 31557600
        planetYearRatio :: Planet -> Float
        planetYearRatio Mercury = 0.2408467
        planetYearRatio Venus = 0.61519726
        planetYearRatio Earth = 1.0
        planetYearRatio Mars = 1.8808158
        planetYearRatio Jupiter = 11.862615
        planetYearRatio Saturn = 29.447498
        planetYearRatio Uranus = 84.016846
        planetYearRatio Neptune = 164.79132