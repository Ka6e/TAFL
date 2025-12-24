-- CircleSquare.hs
module Main where

main :: IO ()
main = do
    putStrLn "Введите радиус круга:"
    input <- getLine
    let r = read input :: Double
        area = pi * r ^ 2
    putStrLn $ "Площадь круга: " ++ show area