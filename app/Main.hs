module Main where

import Anagram (anagramsFor)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ anagramsFor "stone" ["stone", "tones", "banana", "tons", "notes", "Seton"]
  print $ anagramsFor "good" ["dog", "goody"]
