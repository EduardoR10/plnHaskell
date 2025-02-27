import System.IO

sumarLista :: [Int] -> Int
sumarLista = sum

factorial :: Int -> Int
factorial 0 = 1
factorial n =n*factorial(n-1)

numerosPares :: Int -> [Int]
numerosPares n = [x | x <- [0..n], even x]

longitudCadena :: String -> Int
longitudCadena =length

reversoLista :: [Int] -> [Int]
reversoLista =reverse

duplicarElementos :: [Int] -> [Int]
duplicarElementos = map(*2)

filtrarPares :: [Int] -> [Int]
filtrarPares = filter even

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1)+fibonacci(n-2)

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

esPalindromo :: String -> Bool
esPalindromo s = s == reverse s

main :: IO ()
main = do
  putStrLn "Elige una opción (número):"
  putStrLn "1. Sumar lista"
  putStrLn "2. Factorial"
  putStrLn "3. Números pares"
  putStrLn "4. Longitud de cadena"
  putStrLn "5. Reverso de lista"
  putStrLn "6. Duplicar elementos"
  putStrLn "7. Filtrar pares"
  putStrLn "8. Fibonacci"
  putStrLn "9. Divisores"
  putStrLn "10. Palindromo"
  opcion <- getLine
  case opcion of
    "1" -> do
      putStrLn "Introduce una lista de enteros:" 
      lista <- readLn :: IO [Int]
      print (sumarLista lista)
    "2" -> do
      putStrLn "Introduce un número entero:" 
      n <- readLn
      print (factorial n)
    "3" -> do
      putStrLn "Introduce un número entero:" 
      n <- readLn
      print (numerosPares n)
    "4" -> do
      putStrLn "Introduce una cadena:" 
      cadena <- getLine
      print (longitudCadena cadena)
    "5" -> do
      putStrLn "Introduce una lista de enteros:" 
      lista <- readLn :: IO [Int]
      print (reversoLista lista)
    "6" -> do
      putStrLn "Introduce una lista de enteros:" 
      lista <- readLn :: IO [Int]
      print (duplicarElementos lista)
    "7" -> do
      putStrLn "Introduce una lista de enteros:" 
      lista <- readLn :: IO [Int]
      print (filtrarPares lista)
    "8" -> do
      putStrLn "Introduce un número entero:" 
      n <- readLn
      print (fibonacci n)
    "9" -> do
      putStrLn "Introduce un número entero:" 
      n <- readLn
      print (divisores n)
    "10" -> do
      putStrLn "Introduce una cadena:" 
      cadena <- getLine
      print (esPalindromo cadena)
    _ -> putStrLn "Opción no válida"
