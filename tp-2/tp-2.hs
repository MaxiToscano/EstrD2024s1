--                      PRÁCTICA 2: LISTAS Y RECURSIÓN ESTRUCTURAL


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

sucesor :: Int -> Int --función tp-1
sucesor n = n+1

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
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

--8. 
{-apariciones' :: Eq a => a -> [a] -> Int
--Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones' _ [] = 0
aparaciones' e (x:xs) = unoSi (e == x) + apariciones' e xs-}

--DUDA: no me funciona con la subtarea

unoSi :: Bool -> Int --función tp-1
unoSi True = 1
unoSi _ = 0

apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0
apariciones e (x:xs) = if e == x
                        then 1 + apariciones e xs
                        else apariciones e xs

--9. 
losMenoresA :: Int -> [Int] -> [Int]
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA _ [] = []
losMenoresA n (x:xs) = agregarSi x (x < n) ++ losMenoresA n xs

agregarSi :: a -> Bool -> [a]
agregarSi x True = x : []
agregarSi _ _  = []


--10. 
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
--Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (x:xs) = agregarSi x (longitud x > n) ++ lasDeLongitudMayorA n xs


--11. 
agregarAlFinal :: [a] -> a -> [a]
--Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal [] e = e : []
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e 


--12. 
agregar :: [a] -> [a] -> [a]
{-Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
elementos de la segunda a continuación. Definida en Haskell como (++).-}
agregar [] xs = xs
agregar (x:xs) ys = x: agregar xs ys

--13. COMPLETAR
reversa :: [a] -> [a]
--Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. 
--Definida en Haskell como reverse.
reversa [] = []
reversa (x:xs) = reversa xs ++ [x] 

--14. 
zipMaximos :: [Int] -> [Int] -> [Int]
{-Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.-}
zipMaximos [] ns = ns 
zipMaximos ns [] = ns
zipMaximos (n:ns) (m:ms) = maxDelPar (n,m) : zipMaximos ns ms

maxDelPar :: (Int, Int) -> Int --función tp-1
maxDelPar (n, m) = if n > m 
                    then n 
                    else m

--15. COMPLETAR
elMinimo :: Ord a => [a] -> a
--Dada una lista devuelve el mínimo
--PRECOND: la lista no es vacía
elMinimo [x] = x
elMinimo (x:xs) = minimoEntre x (elMinimo xs)

minimoEntre :: Ord a => a -> a -> a
minimoEntre x y = if x < y 
                    then x
                    else y


-- /////////////////////////////////////////////////////////////////////////////////////////////////

-- PUNTO 2: Recursión sobre números

--Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique lo contrario:

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
sinLosPrimeros 0 xs = xs
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

-- //////////////////////////////////////////////////////////////////////////////////////////////

-- PUNTO3: Registros

--1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:

data Persona = P String Int 
--               nombre edad
    deriving Show

homero, marge, bart, lisa, maggie :: Persona
homero = P "Homero" 39
marge  = P "Marge"  38
bart   = P "Bart"   10
lisa   = P "Lisa"   8
maggie = P "Maggie" 2

edad :: Persona -> Int  --función tp-1
--Devuelve la edad de una persona
edad (P n e) = e


mayoresA :: Int -> [Persona] -> [Persona]
--Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA _  [] = []
mayoresA n (p:ps) = agregarSi p (edad p > n) ++ mayoresA n ps


promedioEdad :: [Persona] -> Int
--Dada una lista de personas devuelve el promedio de edad entre esas personas. 
--Precondición: la lista al menos posee una persona.
promedioEdad ps = div (sumatoria (edades ps)) (longitud ps)

edades :: [Persona] -> [Int]
--Dada una lista de personas devuelve una lista con sus edades
edades [] = []
edades (p:ps) = edad p : edades ps


elMasViejo :: [Persona] -> Persona
--Dada una lista de personas devuelve la persona más vieja de la lista. 
--Precondición: la lista al menos posee una persona.
elMasViejo [p] = p
elMasViejo (p:ps) = laQueEsMayor p (elMasViejo ps)

esMayorQueLaOtra :: Persona -> Persona -> Bool  --función tp-1
--Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona   --función tp-1
--Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2 
                        then p1 
                        else p2


-- ////////////////////////////////////////////////////////////////////////////////////////////////

--2. Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente manera:

data TipoDePokemon = Agua | Fuego | Planta 
    deriving Show

data Pokemon = PM TipoDePokemon Int
--                              %energia
    deriving Show

data Entrenador = E String [Pokemon]
--                  nombre /lista de sus pokemones
    deriving Show

charmander, squirtle, bulbasaur, chicorita :: Pokemon
charmander = PM Fuego 70
squirtle = PM Agua 80
bulbasaur = PM Planta 90
chicorita = PM Planta 50

ash :: Entrenador
ash = E "Ash" [charmander, bulbasaur, squirtle]

gari :: Entrenador
gari = E "Gari" [bulbasaur, chicorita]

--Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
--Definir en base a esa representación las siguientes funciones:

cantPokemon :: Entrenador -> Int
--Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon e = longitud (pokemonesDe e)

pokemonesDe :: Entrenador -> [Pokemon]
--dado un entrenador devuelve la lista de sus pokemones
pokemonesDe (E _ pms) = pms

-- ------------------------------------------------------------------------------------------------
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe tp (E _ pms) = longitud (pokemonesTipo tp pms)


pokemonesTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
--Dado un TipoDePokemon y una lista Pokemon devuelve una lista de pokemones del TipoDePokemon dado
pokemonesTipo _ [] = []
pokemonesTipo tp (pm:pms) = if (sonTiposIguales tp (pokemonTipo pm)) 
                            then pm : pokemonesTipo tp pms
                            else pokemonesTipo tp pms

sonTiposIguales :: TipoDePokemon -> TipoDePokemon -> Bool   --función tp-1
sonTiposIguales Agua Agua = True
sonTiposIguales Fuego Fuego = True 
sonTiposIguales Planta Planta = True 
sonTiposIguales _ _ = False 

pokemonTipo :: Pokemon -> TipoDePokemon --función tp-1
--dado un pokemon devuelve su tipo
pokemonTipo (PM tp _) = tp

-- -----------------------------------------------------------------------------------------------

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
{-Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
a los Pokemon del segundo entrenador.-}
cuantosDeTipo_De_LeGananATodosLosDe_ tp e1 e2 = if esTipoSuperiorATodosLosPokemones tp (pokemonesDe e2)
                                                then cantPokemonDe tp e1 
                                                else 0

esTipoSuperiorATodosLosPokemones :: TipoDePokemon -> [Pokemon] -> Bool
--Dados un Pokemon y una lista de pokemones indica si el pokemin dado vence a todos los de la lista según su tipo
esTipoSuperiorATodosLosPokemones _ [] = True
esTipoSuperiorATodosLosPokemones tp (pm:pms) = tiposuperaAPokemon tp pm && esTipoSuperiorATodosLosPokemones tp pms
 
tiposuperaAPokemon :: TipoDePokemon -> Pokemon -> Bool
{-Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.-}
tiposuperaAPokemon tp pm = tipoVenceA tp (pokemonTipo pm)

tipoVenceA :: TipoDePokemon -> TipoDePokemon -> Bool    --función tp-1
--dado dos TipoDePokemon indica si el primero vence al segundo
tipoVenceA Agua Fuego = True
tipoVenceA Fuego Planta = True
tipoVenceA Planta Agua = True
tipoVenceA _ _ = False

-- ------------------------------------------------------------------------------------------------

esMaestroPokemon :: Entrenador -> Bool
--Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon (E _ pms) = hayPokemonTipo Agua pms  && 
                             hayPokemonTipo Fuego pms &&
                             hayPokemonTipo Planta pms

hayPokemonTipo :: TipoDePokemon -> [Pokemon] -> Bool
hayPokemonTipo _ [] = False
hayPokemonTipo tp (pm:pms) = sonTiposIguales tp (pokemonTipo pm) || hayPokemonTipo tp pms


-- ////////////////////////////////////////////////////////////////////////////////////////////////

--PUNTO 3. 

{-El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
una lista de personas con diferente rol.-}
--La definición es la siguiente:

data Seniority = Junior | SemiSenior | Senior 
    deriving Show
data Proyecto = ConsProyecto String 
    deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto 
    deriving Show
data Empresa = ConsEmpresa [Rol] 
    deriving Show

paginaWeb, baseDeDatos, frontEnd, backEnd :: Proyecto
paginaWeb = ConsProyecto "Pagina Web"
baseDeDatos = ConsProyecto "baseDeDatos"
frontEnd = ConsProyecto "Front End"
backEnd = ConsProyecto "Back End"

bill, steve, mark, jeff, elon :: Rol
bill  = Developer  SemiSenior frontEnd
steve = Management Senior     backEnd 
mark  = Developer  Senior     baseDeDatos
jeff  = Management SemiSenior baseDeDatos
elon  = Developer  Junior     paginaWeb

multiEmpresa :: Empresa
multiEmpresa = ConsEmpresa [bill, steve, mark, jeff, elon]

--Definir las siguientes funciones sobre el tipo Empresa:

proyectos :: Empresa -> [Proyecto] -- COMPLETAR
--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos  (ConsEmpresa rs) = proyectosDeRoleSinRepetir rs

proyectosDeRoleSinRepetir :: [Rol] -> [Proyecto]
proyectosDeRoleSinRepetir [] = []
proyectosDeRoleSinRepetir rs = proyectosSinRepetir (proyectosDeRoles rs) 

proyectosSinRepetir :: [Proyecto] -> [Proyecto]
proyectosSinRepetir [] = []
proyectosSinRepetir (p:ps) = agregarSi p (not (estaProyectoEnLaLista p ps)) ++ (proyectosSinRepetir ps)

estaProyectoEnLaLista :: Proyecto -> [Proyecto] -> Bool
estaProyectoEnLaLista p [] = False
estaProyectoEnLaLista p (py:pys) = sonMismoProyecto p py || estaProyectoEnLaLista p pys

proyectosDeRoles :: [Rol] -> [Proyecto]
--Dada una lista de roles devuelve una lista de los proyectos que tienen
proyectosDeRoles [] = []
proyectosDeRoles (r:rs) = proyectoDeRol r : proyectosDeRoles rs

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ p) = p 
proyectoDeRol (Management _ p) = p

sonMismoProyecto :: Proyecto -> Proyecto -> Bool
sonMismoProyecto p1 p2 = nombreDelproyecto p1 == nombreDelproyecto p2

nombreDelproyecto :: Proyecto -> String
nombreDelproyecto (ConsProyecto n) = n

-- -------------------------------------------------------------------------------------------------

losDevSenior :: Empresa -> [Proyecto] -> Int
--Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen además a los proyectos dados por parámetro.
losDevSenior _ [] = 0
losDevSenior e pys = longitud (desarrolladoresSeniorQueTrabajanEn (rolesdeEmpresa e) pys)

rolesdeEmpresa :: Empresa -> [Rol]
rolesdeEmpresa (ConsEmpresa rs) = rs

desarrolladoresSeniorQueTrabajanEn :: [Rol] -> [Proyecto] -> [Rol]
desarrolladoresSeniorQueTrabajanEn [] _ = []
desarrolladoresSeniorQueTrabajanEn _ [] = []
desarrolladoresSeniorQueTrabajanEn (r:rs) (p:ps) = agregarSi r (esDesarroladorSeniorYTrabajaEn r p) 
                                                  ++ desarrolladoresSeniorQueTrabajanEn rs ps

esDesarroladorSeniorYTrabajaEn :: Rol -> Proyecto -> Bool
esDesarroladorSeniorYTrabajaEn r p = esDesarrolladorSenior r && trabajaEnProyecto r p

esDesarrolladorSenior :: Rol -> Bool
esDesarrolladorSenior (Developer Senior _) = True
esDesarrolladorSenior _                    = False

trabajaEnProyecto :: Rol -> Proyecto -> Bool
trabajaEnProyecto (Developer _ p) py = sonMismoProyecto p py
trabajaEnProyecto (Management _ p) py = sonMismoProyecto p py
 
-- -------------------------------------------------------------------------------------------------
{-}
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas.-}
