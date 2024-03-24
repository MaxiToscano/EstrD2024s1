--                      PRÁCTICA 2: LISTAS Y RECURSIÓN ESTRUCTURAL


sucesor :: Int -> Int 
sucesor n = n+1

daUnoSiCumple :: Bool -> Int 
daUnoSiCumple True = 1
daUnoSiCumple _ = 0

agregarSi :: a -> Bool -> [a] -> [a]
agregarSi x True l = x : l
agregarSi _ _ l = l

maxDelPar :: (Int, Int) -> Int 
maxDelPar (n, m) = if n > m 
                    then n 
                    else m

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

--DUDA: no me funciona con la subtarea

apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = if e == x
                        then 1 + apariciones e xs
                        else apariciones e xs

--9. 
{-losMenoresA :: Int -> [Int] -> [Int]
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA n [] = []
losMenoresA n (x:xs) = if x < n 
                        then x : losMenoresA n xs
                        else losMenoresA n xs
-}

losMenoresA :: Int -> [Int] -> [Int]
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA n [] = []
losMenoresA n (x:xs) = agregarSi x (x < n) (losMenoresA n xs)


--10. 
{-
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
--Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) = if (length x) > n
                                then x : lasDeLongitudMayorA n xs
                                else lasDeLongitudMayorA n xs
-}

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
--Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) = agregarSi x (length x > n) (lasDeLongitudMayorA n xs)


--11. 
agregarAlFinal :: [a] -> a -> [a]
--Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal [] e = e : []
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e 


agregarAlFinal' :: [a] -> a -> [a]
--Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal' [] e = e : []
agregarAlFinal' l  e = l ++ [e]


--12. 
agregar :: [a] -> [a] -> [a]
{-Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
elementos de la segunda a continuación. Definida en Haskell como (++).-}
agregar [] xs = xs
agregar (x:xs) ys = x: agregar xs ys

--13. 
{-reversa :: [a] -> [a]
--Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. 
--Definida en Haskell como reverse.
reversa [] = []
reversa (x:xs) = -}

--14. 
zipMaximos :: [Int] -> [Int] -> [Int]
{-Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.-}
zipMaximos [] ns = ns 
zipMaximos ns [] = ns
zipMaximos (n:ns) (m:ms) = maxDelPar (n,m) : zipMaximos ns ms

--15. 
{-elMinimo :: Ord a => [a] -> a
--Dada una lista devuelve el mínimo
elMinimo [] = []
elMinimo (x:xs) = -}


-- /////////////////////////////////////////////////////////////////////////////////////////////////

-- PUNTO 2: Recursión sobre números

--DeFIna las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique lo contrario:

--1. 
factorial :: Int -> Int
{-Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.-}
--PRECOND: n no es negativo 
factorial 0 = 1
factorial n = n * factorial (n-1)

--2. 
cuentaRegresiva :: Int -> [Int]
{-Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.-}
cuentaRegresiva n = if n < 1 
                    then []
                    else n : cuentaRegresiva (n-1)

--3. 
repetir :: Int -> a -> [a]
--Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir 0 e = []
repetir n e = e : repetir (n-1) e


--4. 
losPrimeros :: Int -> [a] -> [a]
{-Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
Si la lista es vacía, devuelve una lista vacía.-}
losPrimeros _ [] = []
losPrimeros 0 _  = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs


--5. 
sinLosPrimeros :: Int -> [a] -> [a]
{-Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
recibida. Si n es cero, devuelve la lista completa-}
sinLosPrimeros _ [] = []
sinLosPrimeros 0 xs = xs 
--sinLosPrimeros n (x:xs) = 