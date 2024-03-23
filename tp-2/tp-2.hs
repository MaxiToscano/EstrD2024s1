--                      PRÁCTICA 2: LISTAS Y RECURSIÓN ESTRUCTURAL


sucesor :: Int -> Int 
sucesor n = n+1

daUnoSiCumple :: Bool -> Int 
daUnoSiCumple True = 1
daUnoSiCumple _ = 0

-- PUNTO 1: Recursión sobre listas

--Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique lo contrario:

--1. 
sumatoria :: [Int] -> Int
--Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

--2. 
longitud :: [a] -> Int
--Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--3. 
sucesores :: [Int] -> [Int]
--Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores [] = []
sucesores (n:ns) = sucesor n : sucesores ns 

--4. 
conjuncion :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion [] = True --la base tiene que ser True porque sino daría False cuando termine la recursión aunque todos los elementos sean True
conjuncion (b:bs) = b && conjuncion bs

--5. 
disyuncion :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion [] = False --la base tiene que False porque sino daría True cuando termine la recursión aunque todos los elementos sean False
disyuncion (b:bs) = b || disyuncion bs

--6. 
aplanar :: [[a]] -> [a]
--Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar [] = []
aplanar (l:ls) = l ++ aplanar ls 


--7. 
pertenece :: Eq a => a -> [a] -> Bool
--Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs

--8. 
{-apariciones :: Eq a => a -> [a] -> Int
--Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones e [] = 0
aparaciones e (x:xs) = daUnoSiCumple (e == x) + apariciones e xs-}

--DUDA: no me sale con la subtarea

apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = if e == x
                        then 1 + apariciones e xs
                        else apariciones e xs

--9. 
losMenoresA :: Int -> [Int] -> [Int]
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA n [] = []
losMenoresA n (x:xs) = if x < n 
                        then x : losMenoresA n xs
                        else losMenoresA n xs


--10. 
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
--Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) = if (length x) > n
                                then x : lasDeLongitudMayorA n xs
                                else lasDeLongitudMayorA n xs

{-11. agregarAlFinal :: [a] -> a -> [a]
Dados una lista y un elemento, devuelve una lista con ese elemento agregado al nal de la
lista.
12. agregar :: [a] -> [a] -> [a]
Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
elementos de la segunda a continuación. Denida en Haskell como (++).
13. reversa :: [a] -> [a]
Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Denida
en Haskell como reverse.
14. zipMaximos :: [Int] -> [Int] -> [Int]
Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.
15. elMinimo :: Ord a => [a] -> a
Dada una lista devuelve el mínimo
-}
