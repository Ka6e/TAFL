-- SumNumbers.hs
module Main where

main :: IO ()
main = do
    contents <- getContents
    let numbers = map read (words contents) :: [Double]
        total   = sum numbers
    putStrLn $ "Сумма чисел: " ++ show total