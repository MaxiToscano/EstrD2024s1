--                              PRÁCTICA 1: TIPOS ALGEBRAICOS

--EJERCICIO 1: Defina las siguientes funciones:

--a) Función sucesor: dado un número devuelve el anterior.

sucesor :: Int -> Int 
sucesor n = n+1

--b) Función sumar: Dados dos números devuelve su suma utilizando la operación +.

sumar :: Int -> Int -> Int
sumar n m = n+m

{-c) Función divisionYResto: Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.-}

divisionYResto :: Int -> Int -> (Int, Int) 
--PRECOND: m no puede ser 0
divisionYResto n m = (div n m, mod n m)

--d) funcion maxDelPar: Dado un par de números devuelve el mayor de estos.

maxDelPar :: (Int, Int) -> Int 
maxDelPar (n, m) = if n > m 
                    then n 
                    else m

-- //////////////////////////////////////////////////////////////////////////////////////////

--EJERCICIO 2: Números enteros

{-
De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresi
ón a todas las funciones del punto anterior.
Ejemplo: maxDelPar (divisionYResto (sumar 5 5) (sucesor 0))


sucesor (sumar (maxDelPar (divisionYResto 8 1)) 1)

divisionYResto 20 1 - maxDelPar (sumar 5 (sucesor 4), 2)

maxDelPar (divisionYResto 1 2) * sucesor (sumar 5 4)

sumar 3 (sucesor 1) + maxDelPar (divisionYResto 10 2)

-}

-- //////////////////////////////////////////////////////////////////////////////////////////

--EJERCICIO 3: Tipos enumerativos


--Punto1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. 

data Dir = Norte | Sur | Este | Oeste
    deriving Show


--Luego implementar las siguientes funciones:

--a) 
opuesto :: Dir -> Dir      
--Dada una dirección devuelve su opuesta.
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

--b) 
iguales :: Dir -> Dir -> Bool
--Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False


--c) 
siguiente :: Dir -> Dir
{-Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¿Posee una precondición esta función? ¿Es una función
total o parcial? ¿Por qué?-}

--PRECOND: la Dir es distina de Oeste 
siguiente Norte = Este 
siguiente Este = Sur 
siguiente Sur = Oeste 
siguiente Oeste = error "No existe siguiente direccion a Oeste"

{-Es una función parcial ya que puede fallar en el caso de la opción Oeste, la cual no posee 
siguiente dirección en este modelo y por este motivo posee una precondición-}

-- //////////////////////////////////////////////////////////////////////////////////////////

{-Punto2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo.-} 

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

--Luego implementar las siguientes funciones:

--a) 
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
{-Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana. Considerar definir subtareas útiles
que puedan servir después.-}
primeroYUltimoDia = (primerDia, ultimoDia)

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

--b) 
empiezaConM :: DiaDeSemana -> Bool
--Dado un día de la semana indica si comienza con la letra M.
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM d = False

--c) 
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
{-Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).-}
vieneDespues d1 d2 = ordenDia d1 > ordenDia d2 
                         

ordenDia :: DiaDeSemana -> Int
ordenDia Lunes = 1
ordenDia Martes = 2
ordenDia Miercoles = 3
ordenDia Jueves = 4
ordenDia Viernes = 5
ordenDia Sabado = 6
ordenDia Domingo = 7

--d) 
estaEnElMedio :: DiaDeSemana -> Bool
--Dado un día de la semana indica si no es ni el primer ni el ultimo dia.

estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

-- //////////////////////////////////////////////////////////////////////////////////////////

{-Punto3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
ya definidas en Haskell):-}

--a) 
negar :: Bool -> Bool
{-Dado un booleano, si es True devuelve False, y si es False devuelve True.
En Haskell ya está definida como not.-}
negar True = False
negar False = True


--b) 
implica :: Bool -> Bool -> Bool
{-Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True.-}
implica True False = False
implica _ _ = True
{-Esta función NO debe realizar doble pattern matching.
Nota: no viene implementada en Haskell.-}


--c) 
yTambien :: Bool -> Bool -> Bool
{-Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como \&\&.-}
yTambien True True = True
yTambien _ _ = False

--d) 
oBien :: Bool -> Bool -> Bool
{-Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como ||.-}
oBien False False = False
oBien _ _ = True

-- //////////////////////////////////////////////////////////////////////////////////////////

--EJERCICIO 4: Registros

--Punto 1: Definir el tipo de dato Persona, como un nombre y la edad de la persona. 
--Realizar las siguientes funciones:

data Persona = P String Int
               --Nombre Edad
    deriving Show

cosme :: Persona
cosme = P "cosme" 30

fulanito :: Persona
fulanito = P "fulanito" 20

nombre :: Persona -> String
--Devuelve el nombre de una persona
nombre (P n e) = n

nombre' :: Persona -> String
--Devuelve el nombre de una persona
nombre' p = nombre p

edad :: Persona -> Int
--Devuelve la edad de una persona
edad (P n e) = e

crecer :: Persona -> Persona
--Aumenta en uno la edad de la persona.
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
--Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.
cambioDeNombre newN (P n e) = (P newN e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
--Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
--Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2 
                        then p1 
                        else p2

-- //////////////////////////////////////////////////////////////////////////////////////////

{-Punto 2: Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. -}

data Pokemon = PM TipoDePokemon Int 
--                              % de energia
    deriving Show

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Entrenador = E String Pokemon Pokemon
--                  nombre
    deriving Show

charmander :: Pokemon
charmander = PM Fuego 70

squirtle :: Pokemon
squirtle = PM Agua 80

bulbasaur :: Pokemon
bulbasaur = PM Planta 90

ash :: Entrenador
ash = E "Ash" charmander bulbasaur



--Luego definir las siguientes funciones:

superaA :: Pokemon -> Pokemon -> Bool
{-Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.-}
superaA pm1 pm2 = tipoVenceA (pokemonTipo pm1) (pokemonTipo pm2)

tipoVenceA :: TipoDePokemon -> TipoDePokemon -> Bool
--dado dos TipoDePokemon indica si el primero vence al segundo
tipoVenceA Agua Fuego = True
tipoVenceA Fuego Planta = True
tipoVenceA Planta Agua = True
tipoVenceA _ _ = False

pokemonTipo :: Pokemon -> TipoDePokemon
--dado un pokemon devuelve su tipo
pokemonTipo (PM tp e) = tp

-- //////////////////////////////////////////////////////////////////////////////////////////

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe tp (E _ pm1 pm2) = daUnoSiCumple (sonTiposIguales tp (pokemonTipo pm1)) +
                                       daUnoSiCumple (sonTiposIguales tp (pokemonTipo pm2)) 

--esPokemonTipo :: Pokemon -> TipoDePokemon -> Bool
--esPokemonTipo pm tp = (pokemonTipo pm) == tp
--error: Instance of Eq TipoDePokemon required for definition of esPokemonTipo

sonTiposIguales :: TipoDePokemon -> TipoDePokemon -> Bool
sonTiposIguales Agua Agua = True
sonTiposIguales Fuego Fuego = True 
sonTiposIguales Planta Planta = True 
sonTiposIguales _ _ = False 


daUnoSiCumple :: Bool -> Int 
daUnoSiCumple True = 1
daUnoSiCumple False = 0

-- //////////////////////////////////////////////////////////////////////////////////////////

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
--Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon (e1, e2) = pokemonesDe e1 ++ pokemonesDe e2

pokemonesDe :: Entrenador -> [Pokemon]
--dado un entrenador devuelve una lista con sus pokemones
pokemonesDe (E _ pm1 pm2) = [pm1, pm2]

-- //////////////////////////////////////////////////////////////////////////////////////////

-- EJERCICIO 5: Funciones polimórficas


--Punto 1: Defina las siguientes funciones polimórficas:

--a) 
loMismo :: a -> a
--Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo x = x

--b) 
siempreSiete :: a -> Int
--Dado un elemento de algún tipo devuelve el número 7.
siempreSiete a = 7

--c) 
swap :: (a, b) -> (b, a)
--Dadas una tupla, invierte sus componentes.
swap (x, y) = (y, x)

-- ¿Por qué existen dos variables de tipo diferentes?
{-Las variables son de tipo diferente porque se pueden pasar datos de diferentes tipos y
asi la función pueda diferenciarlas y cumplir su propósito-}

--Punto 2: Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas?

{-Son polimóficas porque funcionan con cualquier tipo de datos, osea que los elementos no 
están restringidos, lo que las hacen funciones genéricas al no importar los datos con los
que opera-}

-- //////////////////////////////////////////////////////////////////////////////////////////

--EJERCICIO 6: Pattern matching sobre listas

{-Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):-}

--Nota: tener en cuenta que el constructor de listas es :

--a)
estaVacia :: [a] -> Bool
--Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
estaVacia [] = True
estaVacia (_:_) = False
--Definida en Haskell como null.

estaVacia' :: [a] -> Bool
--Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
estaVacia' [] = True
estaVacia' _ = False


--b) 
elPrimero :: [a] -> a
--Dada una lista devuelve su primer elemento.
--PRECOND: la lista no está vacía.
elPrimero (x:_) = x
elPrimero [] = error "Es una lista vacia" 
--Definida en Haskell como head.


--c)
sinElPrimero :: [a] -> [a]
--Dada una lista devuelve esa lista menos el primer elemento.
--PRECOND: la lista no está vacía.
sinElPrimero (_:xs) = xs
sinElPrimero [] = error "Es una lista vacia" 
--Definida en Haskell como tail.


--d)
splitHead :: [a] -> (a, [a])
{-Dada una lista devuelve un par, donde el primer componente es el primer elemento de la
lista, y el segundo componente es esa lista pero sin el primero.-}
--PRECOND: la lista no está vacía.
splitHead l = (elPrimero l, sinElPrimero l)
splitHead _ = error "Es una lista vacia"


