-- FizzBuzz.hs
module Main where

import Control.Monad (unless)

main :: IO ()
main = loop
  where
    loop = do
        eof <- isEOF
        unless eof $ do
            line <- getLine
            let n = read line :: Int
            putStrLn (fizzBuzz n)
            loop

fizzBuzz :: Int -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 3  == 0 = "Fizz"
    | n `mod` 5  == 0 = "Buzz"
    | otherwise       = show n