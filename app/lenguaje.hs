{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

--Función que separa las palabras de una cadena de texto
separarPalabras :: T.Text -> [T.Text]
separarPalabras = T.words

-- Función principal
main :: IO ()
main = do
    putStrLn "Introduce un texto:"
    texto <- TIO.getLine
    
    -- Separar las palabras
    let palabras = separarPalabras texto
    
    -- Imprimir las palabras separadas
    putStrLn "\nPalabras separadas:"
    mapM_ TIO.putStrLn palabras
