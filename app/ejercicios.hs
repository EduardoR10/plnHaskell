import System.IO
import Data.Char(toUpper)

aplicarDescuento :: Double -> Double -> Double
aplicarDescuento precio porcentaje=precio*(1-porcentaje/100)

aplicarIVA :: Double -> Double -> Double
aplicarIVA precio porcentaje=precio*(1+porcentaje/100)

precioFinal :: [(Double, Double)] -> (Double -> Double -> Double) -> Double
precioFinal cesta funcion=sum[funcion precio porcentaje|(precio, porcentaje)<-cesta]

aplicarALista :: (a -> b) -> [a] -> [b]
aplicarALista f lista=map f lista

longitudPalabras :: String -> [(String, Int)]
longitudPalabras frase=[(palabra,length palabra)|palabra<-words frase]

calificar :: Double -> String
calificar nota|nota>=95="Excelente"|nota>=85="Notable"|nota>=75="Bueno"|nota>=70="Suficiente"|otherwise="Desempeño insuficiente"

calificaciones :: [(String, Double)] -> [(String, String)]
calificaciones notas=[(map toUpper asignatura,calificar nota)|(asignatura,nota)<-notas]

moduloVector :: [Double] -> Double
moduloVector vector=sqrt(sum[x^2|x<-vector])

media :: [Double] -> Double
media xs = sum xs / fromIntegral (length xs)

desviacionTipica :: [Double] -> Double
desviacionTipica xs = sqrt (sum [(x - m)^2 | x <- xs] / fromIntegral (length xs))
  where m = media xs

puntuacionTipica :: [Double] -> [Double]
puntuacionTipica xs = [(x - m) / d | x <- xs]
  where m = media xs
        d = desviacionTipica xs
valoresAtipicos :: [Double] -> [Double]
valoresAtipicos xs = [x | (x,z) <- zip xs (puntuacionTipica xs), z > 2.8 || z < -2.8]


main :: IO ()
main = do
  putStrLn"Elige una opción (no.):"
  putStrLn"1. Aplicar descuento"
  putStrLn"2. Aplicar IVA"
  putStrLn"3. Precio final de carrito"
  putStrLn"4. Aplicar a lista funcion"
  putStrLn"5. Longitud de palabras"
  putStrLn"6. Calificaciones"
  putStrLn"7. Módulo de vector"
  putStrLn"8. Valores atipicos"
  opcion<-getLine
  case opcion of
    "1"->do
      putStrLn "Introduce el precio:"
      precio<-readLn
      putStrLn"Introduce el porcentaje de descuento:"
      porcentaje<-readLn
      print(aplicarDescuento precio porcentaje)
    "2"->do
      putStrLn"Introduce el precio:"
      precio<-readLn
      putStrLn"Introduce el porcentaje de IVA:"
      porcentaje<-readLn
      print(aplicarIVA precio porcentaje)
    "3"->do
      putStrLn"Introduce la cesta de compra [(precio, porcentaje)]:"
      cesta<-readLn::IO[(Double,Double)]
      putStrLn"Elige que aplicar (1 para Descuento, 2 para IVA):"
      fun<-getLine
      let funcion=if fun=="1"then aplicarDescuento else aplicarIVA
      print(precioFinal cesta funcion)
    "4"->do
      putStrLn"Introduce una lista de enteros:"
      lista<-readLn::IO[Int]
      print(aplicarALista(*2) lista)
    "5"->do
      putStrLn"Introduce una frase:"
      frase<-getLine
      print(longitudPalabras frase)
    "6"->do
      putStrLn"Introduce las asignaturas y notas [(Asignatura,nota)]:"
      notas<-readLn::IO[(String,Double)]
      print(calificaciones notas)
    "7"->do
      putStrLn"Introduce un vector [x1,x2,..]:"
      vector<-readLn::IO[Double]
      print(moduloVector vector)
    "8"->do
      putStrLn"Introduce una muestra de datos:"
      muestra<-readLn::IO[Double]
      putStrLn "Puntuaciones típicas:"
      print (puntuacionTipica muestra)
      putStrLn "Valores atípicos:"
      print (valoresAtipicos muestra)
    _->putStrLn"Opción no valida"
