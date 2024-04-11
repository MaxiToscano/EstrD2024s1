--                      PRÁCTICA N°4 Ejercicios Integradores

--PUNTO 1. Pizzas
--Tenemos los siguientes tipos de datos:

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

pizza1, pizza2:: Pizza
pizza1 = Capa (Aceitunas 8) (Capa Jamon (Capa Queso (Capa Salsa Prepizza)))
pizza2 = Capa Queso (Capa Salsa Prepizza)

--------------------------------------------------------------------------------------------------

--Definir las siguientes funciones:

cantidadDeCapas :: Pizza -> Int
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

-------------------------------------------------------

armarPizza :: [Ingrediente] -> Pizza
--Dada una lista de ingredientes construye una pizza
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

------------------------------------------------------

sacarJamon :: Pizza -> Pizza
--Le saca los ingredientes que sean jamón a la pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) = if (esIngrediente Jamon i)
                        then sacarJamon p
                        else Capa i (sacarJamon p)

esIngrediente :: Ingrediente -> Ingrediente -> Bool
esIngrediente Salsa Salsa = True
esIngrediente Queso Queso = True
esIngrediente Jamon Jamon = True
esIngrediente (Aceitunas _) (Aceitunas _) = True
esIngrediente _ _ = False

---------------------------------------------------------

tieneSoloSalsaYQueso :: Pizza -> Bool
--Dice si una pizza tiene solamente salsa y queso, o sea no tiene de otros ingredientes. 
--En particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa i p) = (esIngrediente Salsa i || esIngrediente Queso i) && tieneSoloSalsaYQueso p

---------------------------------------------------------
                                 
duplicarAceitunas :: Pizza -> Pizza
--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = Capa (dobleDeAceitunas i) p

dobleDeAceitunas :: Ingrediente -> Ingrediente
dobleDeAceitunas (Aceitunas n) = Aceitunas (n*2)
dobleDeAceitunas i = i

------------------------------------------------------------

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
{-Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
ingredientes de la pizza, y la respectiva pizza como segunda componente-}
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps

-- //////////////////////////////////////////////////////////////////////////////////////////////

--PUNTO 2: Mapa de tesoros (con bifurcaciones)

--Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. 
--Cada bifurcación y cada cofre tiene un objeto, que puede ser chatarra o un tesoro.

data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

mapa1 :: Mapa 
mapa1 = Bifurcacion cofre2 (Bifurcacion cofre2 
                                (Bifurcacion cofre2 
                                    (Fin cofre2) (Fin cofre1)) 
                                (Fin cofre2)
                            )
                           (Fin cofre2)

cofre1 :: Cofre 
cofre1 = Cofre [piedra, espada]
cofre2 = Cofre [cuchara, piedra]

espada, cuchara, piedra :: Objeto
espada = Tesoro 
cuchara = Chatarra
piedra = Chatarra

--Definir las siguientes operaciones:

--1. 
hayTesoro :: Mapa -> Bool
--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro (Fin c) = esCofreConTesoro c
hayTesoro (Bifurcacion c m1 m2) = esCofreConTesoro c || hayTesoro m1 || hayTesoro m2

esCofreConTesoro :: Cofre -> Bool
esCofreConTesoro (Cofre obs) = hayTesoroEnObjetos obs

hayTesoroEnObjetos :: [Objeto] -> Bool
--Indica si en la lista de Objetos dada hay algún tesoro
hayTesoroEnObjetos [] = False
hayTesoroEnObjetos (ob:obs) = esTesoro ob || hayTesoroEnObjetos obs

esTesoro :: Objeto -> Bool
--Indica si el Objeto dado es un tesoro
esTesoro Tesoro = True
esTesoro _      = False

------------------------------------------------------------------------------

--2. 
hayTesoroEn :: [Dir] -> Mapa -> Bool
--Indica si al final del camino hay un tesoro. 
--Nota: el final de un camino se representa con una lista vacía de direcciones.
hayTesoroEn [] m = hayTesoroEnEstaPosicion m
hayTesoroEn _ (Fin _) = error "no hay camino"
hayTesoroEn (d:ds) (Bifurcacion _ m1 m2) = if esIzquierda d 
                                           then hayTesoroEn ds m1
                                           else hayTesoroEn ds m2

hayTesoroEnEstaPosicion :: Mapa -> Bool
hayTesoroEnEstaPosicion (Fin c) = esCofreConTesoro c
hayTesoroEnEstaPosicion (Bifurcacion c _ _) = esCofreConTesoro c

esIzquierda :: Dir -> Bool 
esIzquierda Izq = True 
esIzquierda _   = False


----------------------------------------------------------------------------

--3. 
caminoAlTesoro :: Mapa -> [Dir]
--Indica el camino al tesoro. 
--Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin _) = []
caminoAlTesoro (Bifurcacion c m1 m2) = if esCofreConTesoro c 
                                       then [] 
                                       else dirAlTesoro m1 ++ caminoAlTesoro m1 ++ caminoAlTesoro m2

dirAlTesoro :: Mapa -> [Dir]
dirAlTesoro m = if hayTesoro m 
                then [Izq] 
                else [Der]


--------------------------------------------------------------------------------

--4. 
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = dirCaminoMasLargo m1 m2 
                                               ++ caminoDeLaRamaMasLarga m1
                                               ++ caminoDeLaRamaMasLarga m2

dirCaminoMasLargo :: Mapa -> Mapa -> [Dir]  
dirCaminoMasLargo m1 m2 = if heightT m1 > heightT m2      
                          then [Izq]      
                          else [Der] 

heightT :: Mapa -> Int 
heightT (Fin _) = 0            
heightT (Bifurcacion _ m1 m2) = 1 + heightT m1 + heightT m2            
                                               
------------------------------------------------------------------------------

--5. 
tesorosPorNivel :: Mapa -> [[Objeto]]
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c) = tesorosEn (objetosDelCofre c) : []
tesorosPorNivel (Bifurcacion c m1 m2) = tesorosEn (objetosDelCofre c) : juntarNiveles (tesorosPorNivel m1) (tesorosPorNivel m2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] yss = yss
juntarNiveles xss [] = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

tesorosEn :: [Objeto] -> [Objeto]
tesorosEn [] = []
tesorosEn (ob:obs) = singularSi ob (esTesoro ob) ++ tesorosEn obs

objetosDelCofre :: Cofre -> [Objeto]
objetosDelCofre (Cofre obs) = obs

singularSi :: a -> Bool -> [a]
singularSi a True = [a]
singularSi _ _ = []

----------------------------------------------------------------------------

--6. 
todosLosCaminos :: Mapa -> [[Dir]]
--Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin _) = [[]]
todosLosCaminos (Bifurcacion _ m1 m2) = consACada Izq (todosLosCaminos m1) ++ consACada Der (todosLosCaminos m2)

consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (xs:xss) = (x:xs) : consACada x xss


-- /////////////////////////////////////////////////////////////////////////////////////////////////////////


--PUNTO 3: Nave Espacial

{-modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
es la siguiente:-}

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String
type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

data Nave = N (Tree Sector)
    deriving Show


nave1 = N (NodeT (S "a" [(Motor 2), (Almacen [Comida, Oxigeno])] ["t1", "t2", "t3"])
            (NodeT (S "b" [LanzaTorpedos, (Motor 2), (Almacen [Torpedo, Combustible])] ["t4", "t5", "t6"])
                (EmptyT)
                (EmptyT)
            )
            (NodeT (S "c" [LanzaTorpedos, (Motor 4), (Almacen [Comida, Oxigeno, Torpedo, Combustible])] ["t7", "t8", "t9", "t10"])
                (EmptyT)
                (EmptyT)
            )
          )

--Implementar las siguientes funciones utilizando recursión estructural:


--1. 
sectores :: Nave -> [SectorId]
--Propósito: Devuelve todos los sectores de la nave.
sectores (N t) = sectoresEn t

sectoresEn :: Tree Sector -> [SectorId]
sectoresEn EmptyT  = []
sectoresEn (NodeT s t1 t2) = [idSector s] ++ sectoresEn t1 ++ sectoresEn t2

idSector :: Sector -> SectorId
idSector (S id _ _) = id


---------------------------------------------------------------------------------------------

--2. 
poderDePropulsion :: Nave -> Int
--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. 
--Nota: el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion (N t) = poderDePropulsionT t

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT EmptyT = 0
poderDePropulsionT (NodeT s t1 t2) = poderDePropulsionEn (componentesDelSector s) + poderDePropulsionT t1 + poderDePropulsionT t2

poderDePropulsionEn :: [Componente] -> Int
poderDePropulsionEn [] = 0
poderDePropulsionEn (c:cs) = poderDelMotor c + poderDePropulsionEn cs

componentesDelSector :: Sector -> [Componente]
componentesDelSector (S _ cs _)  = cs

poderDelMotor :: Componente -> Int
poderDelMotor (Motor n) = n
poderDelMotor _         = 0

----------------------------------------------------------------------------------------------

--3. 
barriles :: Nave -> [Barril]
--Propósito: Devuelve todos los barriles de la nave.
barriles (N t) = barrilesT t

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT s t1 t2) = barrilesEnComps (componentesDelSector s) ++ barrilesT t1 ++ barrilesT t2

barrilesEnComps :: [Componente] -> [Barril]
barrilesEnComps [] = []
barrilesEnComps (c:cs) = barrilesDeAlmacen c ++ barrilesEnComps cs

barrilesDeAlmacen :: Componente -> [Barril]
barrilesDeAlmacen (Almacen bs) = bs 
barrilesDeAlmacen _ = []

-------------------------------------------------------------------------------------------------

--4. 
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector [] _ n = n



{-5. asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
Propósito: Incorpora un tripulante a una lista de sectores de la nave.
Precondición: Todos los id de la lista existen en la nave.
6. sectoresAsignados :: Tripulante -> Nave -> [SectorId]
Propósito: Devuelve los sectores en donde aparece un tripulante dado.-}