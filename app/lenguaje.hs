{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Data.Char (isPunctuation, toLower)

--Función que separa palabras de un texto
separarPalabras :: T.Text -> [T.Text]
separarPalabras = T.words

--Función que convierte todo el texto a minúsculas
aMinusculas :: T.Text -> T.Text
aMinusculas = T.toLower

--Función que elimina signos de puntuación
eliminarPuntuacion :: T.Text -> T.Text
eliminarPuntuacion = T.filter (not . isPunctuation)

--Función que cuenta las palabras únicas
contarPalabrasUnicas :: [T.Text] -> Int
contarPalabrasUnicas palabras = length (Map.keys (contarFrecuencia palabras))

--Función que cuenta la frecuencia de cada palabra
contarFrecuencia :: [T.Text] -> Map.Map T.Text Int
contarFrecuencia = foldr (\palabra -> Map.insertWith (+) palabra 1) Map.empty

main :: IO ()
main = do
    putStrLn "Introduce un texto:"
    texto <- TIO.getLine

    --Convertir a minúsculas y eliminar puntuación
    let textoLimpio = eliminarPuntuacion (aMinusculas texto)
    
    --Separar las palabras
    let palabras = separarPalabras textoLimpio

    --Contar palabras únicas
    let totalPalabrasUnicas = contarPalabrasUnicas palabras
    
    --Contar frecuencia de palabras
    let frecuencia = contarFrecuencia palabras

    putStrLn "\nPalabras separadas:"
    mapM_ TIO.putStrLn palabras

    putStrLn $ "\nNúmero total de palabras únicas: " ++ show totalPalabrasUnicas

    putStrLn "\nFrecuencia de palabras:"
    mapM_ (\(pal, freq) -> TIO.putStrLn (pal <> ": " <> T.pack (show freq))) (Map.toList frecuencia)
