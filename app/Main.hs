module Main where

fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 3 == 0 && n `mod` 5 == 0 = "fizzbuzz"
    | n `mod` 3 == 0 = "buzz"
    | n `mod` 5 == 0 = "fizz"
    | n > 0 && n < 20 = 
        let unicos = words "one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen" 
        in unicos !! (n - 1)
    | n >= 20 = numeros n
    | otherwise = "Fuera del rango"

numeros :: Int -> String
numeros n
    | n `mod` 10 == 0 = 
        let numero = words "twenty thirty forty fifty sixty seventy eighty ninety hundred" 
        in numero !! ((div n 10) - 2) 
    | otherwise = 
        let numero = words "twenty thirty forty fifty sixty seventy eighty ninety"
            unidades = words "one two three four five six seven eight nine"
        in numero !! ((div n 10) - 2) ++ " " ++ unidades !! ((mod n 10) - 1)

main :: IO ()
main = do
    putStrLn "Dame un numero:"
    input <- getLine
    let n = read input :: Int
    if n < 1 || n > 100
        then putStrLn "Solo admite del 1 al 100"
        else putStrLn (fizzbuzz n ++ "!")