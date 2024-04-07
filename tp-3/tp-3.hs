--                                  PRÁCTICA 3: TIPOS RECURSIVOS


--1. Tipos recursivos simples

--PUNTO 1.1: Celdas con bolitas
--Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:

data Color = Azul | Rojo
    deriving Show

data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

celda1 :: Celda 
celda1 = Bolita Azul (Bolita Azul (Bolita Rojo (CeldaVacia)))


{-En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
ser la siguiente:
Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
Implementar las siguientes funciones sobre celdas:
-}

nroBolitas :: Color -> Celda -> Int
--Dados un color y una celda, indica la cantidad de bolitas de ese color. 
--Nota: pensar si ya existe una operación sobre listas que ayude a resolver el problema.
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita co ce) = unoSi (sonColoresIguales c co) + nroBolitas c ce

sonColoresIguales :: Color -> Color -> Bool
sonColoresIguales Azul Azul = True
sonColoresIguales Rojo Rojo = True
sonColoresIguales _    _    = False 

unoSi :: Bool -> Int 
unoSi True = 1
unoSi _ = 0

-- ----------------------------------------------------------------------------------------------

poner :: Color -> Celda -> Celda
--Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner c ce = Bolita c ce


sacar :: Color -> Celda -> Celda
--Dado un color y una celda, quita una bolita de dicho color de la celda. 
--Nota: a diferencia de Gobstones, esta función es total.
sacar c (Bolita co ce) = if sonColoresIguales c co
                         then ce
                         else Bolita co (sacar c ce)


ponerN :: Int -> Color -> Celda -> Celda
--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN 0 _ ce  = ce
ponerN n c ce = poner c (ponerN (n-1) c ce)

-- ////////////////////////////////////////////////////////////////////////////////////////////////

--PUNTO 1.2: Camino hacia el tesoro

--Tenemos los siguientes tipos de datos

data Objeto = Cacharro | Tesoro
    deriving Show

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

camino1, camino2, camino3 :: Camino 
camino1 = Fin
camino2 = Nada (Cofre [espada, cuchara] camino1)
camino3 = Cofre [cuchara, piedra] (Nada (Nada (Nada (Cofre [espada] Fin))))

espada, cuchara, piedra :: Objeto
espada = Tesoro 
cuchara = Cacharro
piedra = Cacharro

--Definir las siguientes funciones:

hayTesoro :: Camino -> Bool
--Indica si hay un cofre con un tesoro en el camino.
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre obs c) = hayTesoroEnObjetos obs || hayTesoro c

hayTesoroEnObjetos :: [Objeto] -> Bool
--Indica si en la lista de Objetos dada hay algún tesoro
hayTesoroEnObjetos [] = False
hayTesoroEnObjetos (ob:obs) = esTesoro ob || hayTesoroEnObjetos obs

esTesoro :: Objeto -> Bool
--Indica si el Objeto dado es un tesoro
esTesoro Tesoro = True
esTesoro _      = False

-- ---------------------------------------------------------------------------------------

pasosHastaTesoro :: Camino -> Int
{-Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.-}
--Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro Fin            = error "tiene que haber al menos un tesoro."
pasosHastaTesoro (Cofre obs c)  = if hayTesoroEnObjetos obs 
                                   then 0
                                   else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c)       = 1 + pasosHastaTesoro c

----------------------------------------------------------------------------------------------

hayTesoroEn :: Int -> Camino -> Bool 
{-Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
pasos es 5, indica si hay un tesoro en 5 pasos.-}
hayTesoroEn _ Fin = False
hayTesoroEn 0 c = esCofreConTesoro c
hayTesoroEn n (Nada c) = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre _ c) = hayTesoroEn (n-1) c

esCofreConTesoro :: Camino -> Bool
esCofreConTesoro (Cofre obs _) = hayTesoroEnObjetos obs
esCofreConTesoro _             = False  


hayTesoroEn' :: Int -> Camino -> Bool --versión hecha en clase práctica
{-Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
pasos es 5, indica si hay un tesoro en 5 pasos.-}
hayTesoroEn' _ Fin           = False  
hayTesoroEn' 0 (Nada c)      = False 
hayTesoroEn' 0 (Cofre obs c) = hayTesoroEnObjetos obs
hayTesoroEn' n c             = hayTesoroEn (n-1) (caminoInterior c)        

caminoInterior :: Camino -> Camino
caminoInterior Fin = error "No puede ser Fin"
caminoInterior (Nada c) = c
caminoInterior (Cofre _ c) = c

----------------------------------------------------------------------------------------------

alMenosNTesoros :: Int -> Camino -> Bool
--Indica si hay al menos n tesoros en el camino.
--PRECOND: n es mayor a 0
alMenosNTesoros n c = cantDeTesorosEn c >= n

cantDeTesorosEn :: Camino -> Int 
cantDeTesorosEn Fin = 0
cantDeTesorosEn (Nada c) = cantDeTesorosEn c 
cantDeTesorosEn (Cofre obs c) = cuantosSonTesoros obs + cantDeTesorosEn c

cuantosSonTesoros :: [Objeto] -> Int 
cuantosSonTesoros [] = 0
cuantosSonTesoros (ob:obs) = unoSi (esTesoro ob) + cuantosSonTesoros obs

-----------------------------------------------------------------------------------------------

--(desafío) COMPLETAR
--cantTesorosEntre :: Int -> Int -> Camino -> Int
{-Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.-}
--cantTesorosEntre n m c = 

cantDeTesorosDesde :: Int -> Camino -> Int 
cantDeTesorosDesde _ Fin = 0
cantDeTesorosDesde 0 (Nada c) = cantDeTesorosEn c
cantDeTesorosDesde 0 (Cofre obs c) = cuantosSonTesoros obs + cantDeTesorosEn c
cantDeTesorosDesde n cam = cantDeTesorosDesde (n-1) (caminoInterior cam)

cantDeTesorosHasta :: Int -> Camino -> Int
cantDeTesorosHasta _ Fin = 0
cantDeTesorosHasta 0 (Nada _) = 0
cantDeTesorosHasta 0 (Cofre obs _) = cuantosSonTesoros obs 
cantDeTesorosHasta n (Nada c) = cantDeTesorosHasta (n-1) c
cantDeTesorosHasta n (Cofre obs c) = cantDeTesorosHasta (n-1) c + cuantosSonTesoros obs 

caminoDesde :: Int -> Camino -> Camino
caminoDesde _ Fin = Fin
caminoDesde 0 (Nada c) = c 
caminoDesde 0 (Cofre _ c) = c
caminoDesde n (Nada c) = caminoDesde (n-1) c
caminoDesde n (Cofre _ c) = caminoDesde (n-1) c


-- //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

--PUNTO 2. Tipos arbóreos


--2.1. Árboles binarios
--Dada esta definición para árboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

treeN :: Tree Int
treeN = NodeT 2 (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT) (NodeT 5 (NodeT 4 (NodeT 3 EmptyT EmptyT) EmptyT) EmptyT)

--defina las siguientes funciones utilizando recursión estructural según corresponda:

--1. 
sumarT :: Tree Int -> Int
--Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2


--2. 
sizeT :: Tree a -> Int
--Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT EmptyT = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

--3. 
mapDobleT :: Tree Int -> Tree Int
--Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n*2) (mapDobleT t1) (mapDobleT t2)

--4. 
perteneceT :: Eq a => a -> Tree a -> Bool
--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT _ EmptyT = False
perteneceT e (NodeT e1 t1 t2) = (e==e1) || perteneceT e t1 || perteneceT e t2

--5. 
aparicionesT :: Eq a => a -> Tree a -> Int
--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
aparicionesT _ EmptyT = 0
aparicionesT e (NodeT e1 t1 t2) = unoSi (e==e1) + aparicionesT e t1 + aparicionesT e t2

--6. 
leaves :: Tree a -> [a]
--Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves EmptyT = []
leaves (NodeT e t1 t2) = e:[] ++ leaves t1 ++ leaves t2

--7. 
heightT :: Tree a -> Int
--Dado un árbol devuelve su altura.
{-Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
de niveles del árbol1. La altura para EmptyT es 0, y para una hoja es 1.-}
heightT EmptyT = 0
heightT (NodeT e t1 t2) = 1 + max (heightT t1) (heightT t2)


--8. 
mirrorT :: Tree a -> Tree a
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho en cada nodo del árbol.
mirrorT EmptyT = EmptyT
mirrorT (NodeT e t1 t2) = NodeT e (mirrorT t2) (mirrorT t1)

--9. 
toList :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz y luego los elementos del hijo derecho.
toList EmptyT = []
toList (NodeT e t1 t2) = toList t1 ++ [e] ++ toList t2


--10. 
levelN :: Int -> Tree a -> [a] 
{-Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
distancia de la raiz a uno de sus hijos es 1.-}
--Nota: El primer nivel de un árbol (su raíz) es 0.
levelN _ EmptyT = []
levelN 0 (NodeT e _ _) = [e]
levelN n (NodeT e t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2


--11. 
listPerLevel :: Tree a -> [[a]]
--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel EmptyT = []
listPerLevel (NodeT e t1 t2) = [e] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] yss = yss
juntarNiveles xss [] = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss


--12. 
ramaMasLarga :: Tree a -> [a]
--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT e t1 t2) = if sizeT t1 > sizeT t2 
                                then e : leaves t1 
                                else e : leaves t2
                            

--13. 
todosLosCaminos :: Tree a -> [[a]]
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos.
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT e t1 t2) = [e] : consACada e (todosLosCaminos t1) ++ consACada e (todosLosCaminos t2)

consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (xs:xss) = (x:xs) : consACada x xss

{-todosLosCaminos (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT)
                         (NodeT 4 (NodeT 5 EmptyT EmptyT) EmptyT))

= [ [1], [1,2], [1,2,3], [1,4], [1,4,5] ]-}

-- ////////////////////////////////////////////////////////////////////////////////////////////////

--Punto 2.2: Expresiones Aritméticas

--El tipo algebraico ExpA modela expresiones aritméticas de la siguiente manera:

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
    deriving Show

expa1 :: ExpA
expa1 = Sum (Prod (Neg (Valor (-4))) (Valor 0)) (Valor 6)

--Implementar las siguientes funciones utilizando el esquema de recursión estructural sobre Exp:

--1. 
eval :: ExpA -> Int
--Dada una expresión aritmética devuelve el resultado evaluarla.
eval (Valor n) = n
eval (Sum e1 e2) = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2    
eval (Neg e) = eval e * (-1)


--2. 
simplificar :: ExpA -> ExpA
--Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando notación matemática convencional):
simplificar (Valor n) = Valor n
simplificar (Sum e1 e2) = sumSimplificada (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2) = prodSimplificado (simplificar e1) (simplificar e2)
simplificar (Neg e) = negSimplificado (simplificar e)

sumSimplificada :: ExpA -> ExpA -> ExpA
sumSimplificada (Valor 0) e2 = e2
sumSimplificada e1 (Valor 0) = e1  
sumSimplificada e1 e2        = Sum e1 e2

prodSimplificado :: ExpA -> ExpA -> ExpA
prodSimplificado (Valor 0) _ = Valor 0
prodSimplificado _ (Valor 0) = Valor 0 
prodSimplificado (Valor 1) e2 = e2
prodSimplificado e1 (Valor 1) = e1
prodSimplificado e1 e2       = Prod e1 e2

negSimplificado :: ExpA -> ExpA
negSimplificado (Neg e) = e
negSimplificado e       = Neg e


{-También existen otras definiciones posibles. Por ejemplo, puede definirse como la distancia del camino desde la
raíz a su hoja más lejana. Por distancia entendemos la cantidad de nodos que hay en dicho camino. En este caso
las hojas tendrían altura 0, porque la distancia del camino a sí mismos lo es. Se suele utilizar más en árboles que
no poseen un constructor vacío.-}

{-
a) 0 + x = x + 0 = x
b) 0 * x = x * 0 = 0
c) 1 * x = x * 1 = x
d) - (- x) = x
-}
