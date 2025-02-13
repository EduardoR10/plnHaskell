module Main where

import Data.List (intercalate)

--verificar si es primo
esPrimo :: Int -> Bool
esPrimo n
    | n < 2     = False
    | otherwise = not (any (\d -> n `mod` d == 0) [2..(floor . sqrt $ fromIntegral n)])

fizzbuzz :: Int -> String
fizzbuzz n
    | esPrimo n = "FizzBuzz!"
    | otherwise = numeroEscrito n

--convertir el nmero a texto
numeroEscrito :: Int -> String
numeroEscrito 0 = "cero"
numeroEscrito 1000000 = "un millón"
numeroEscrito n
    | n < 30     = unidades !! n
    | n < 100    = let (d, u) = n `divMod` 10
                   in if u == 0 
                      then decenas !! (d - 3) 
                      else decenas !! (d - 3) ++ " y " ++ unidades !! u
    | n < 1000   =
        let (c, resto) = n `divMod` 100
            centena = if c == 1 && resto == 0 then "cien" else centenas !! (c - 1)
        in if resto == 0 then centena else centena ++ " " ++ numeroEscrito resto
    | n < 1000000 =
        let (miles, resto) = n `divMod` 1000
            milesStr = if miles == 1 then "mil" else numeroEscrito miles ++ " mil"
        in if resto == 0 
           then milesStr 
           else milesStr ++ " " ++ numeroEscrito resto
    | otherwise  = "Fuera de rango"

--Listas de nombres de números
unidades :: [String]
unidades = ["cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve",
            "diez", "once", "doce", "trece", "catorce", "quince", "dieciséis", "diecisiete",
            "dieciocho", "diecinueve", "veinte", "veintiuno", "veintidós", "veintitrés",
            "veinticuatro", "veinticinco", "veintiséis", "veintisiete", "veintiocho", "veintinueve"]

decenas :: [String]
decenas = ["treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]

centenas :: [String]
centenas = ["ciento", "doscientos", "trescientos", "cuatrocientos", "quinientos", 
            "seiscientos", "setecientos", "ochocientos", "novecientos"]

main :: IO ()
main = do
    putStrLn "Dame un número entre 0 y 1000000:"
    input <- getLine
    let n = read input :: Int
    if n < 0 || n > 1000000
        then putStrLn "Debe ser un número entre 0 y 1000000"
        else putStrLn (fizzbuzz n)
