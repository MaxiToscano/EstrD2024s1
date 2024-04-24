--                          TRABAJO PRÁCTICO N°5 : Set, Stack y Queue



--PUNTO 1. Cálculo de costos


--Especficar el costo operacional de las siguientes funciones:


--Costo Constante: O(1) porque solo opera sobre el primer elemento de la lista.
head' :: [a] -> a  
head' (x:xs) = x


--Costo Constante: O(1) porque solo utiliza la operación constante '+'
sumar :: Int -> Int  
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1


--Costo Lineal: O(n) -> n = el num dado. Porque utiliza la operacion constante '*' en la recursión sobre n
factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)


--Costo Lineal: O(n) -> porque hace una operación de costo constante '+' por cada elemento de la lsita dada.
--n = la longitud de  la lista.
longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


--Costo Cuadrático: O(n^2) -> porque utiliza una función de costo lineal (factorial) por cada num de la lista dada.
--n = longitud de la lista.
factoriales :: [Int] -> [Int] 
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs


--Costo Lineal: O(n) -> n = la longitud de la lista
pertenece :: Eq a => a -> [a] -> Bool 
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs


--Costo Cuadrático: O(n^2) -> porque utiliza una función de costo lineal por cada num de la lista dada.
sinRepetidos :: Eq a => [a] -> [a] 
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs


--Costo Lineal: O(n) -> n = longitud de la primera lista.
-- equivalente a (++)
append :: [a] -> [a] -> [a] 
append [] ys = ys
append (x:xs) ys = x : append xs ys


--Costo Cuadrático: O(n^2) -> porque utiliza una función de costo lineal por cada num de la lista dada.
concatenar :: [String] -> String 
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs


--Costo Lineal: O(n) -> n = num dado
takeN :: Int -> [a] -> [a] 
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs


--Costo Lineal: O(n) -> n = num dado
dropN :: Int -> [a] -> [a] 
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs


--Costo Lineal: O(n) -> n = num dado (porque no usa las funciones en cada elemento de la lista)
partir :: Int -> [a] -> ([a], [a]) 
partir n xs = (takeN n xs, dropN n xs)


--Costo Lineal: O(n) -> n = longitud de la lista. (min compara dos elementos por lo cual es de costo constante)
minimo :: Ord a => [a] -> a 
minimo [x] = x
minimo (x:xs) = min x (minimo xs)


--Costo Lineal. O(n) -> n = longitud de la lista. 
sacar :: Eq a => a -> [a] -> [a] 
sacar n [] = []
sacar n (x:xs) = if n == x
                 then xs
                 else x : sacar n xs


--Costo Cuadrático: O(n^2) -> n = longitud de la lista.
ordenar :: Ord a => [a] -> [a] 
ordenar [] = []
orderar xs = let m = minimo xs
             in m : ordenar (sacar m xs)


-- //////////////////////////////////////////////////////////////////////////////////////////////




