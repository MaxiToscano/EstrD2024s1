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

--(desafío) 
cantTesorosEntre :: Int -> Int -> Camino -> Int
{-Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.-}
cantTesorosEntre n1 n2 c = if pasosHastaTesoro c > n2 
                            then 0
                            else cantDeTesorosEntre n1 n2 c

cantDeTesorosEntre :: Int -> Int -> Camino -> Int

|
 
cantDePasosEn :: Camino -> Int
cantDePasosEn Fin = 0
cantDePasosEn (Nada c) = 1 + cantDePasosEn c
cantDePasosEn (Cofre _ c) = 1 + cantDePasosEn c