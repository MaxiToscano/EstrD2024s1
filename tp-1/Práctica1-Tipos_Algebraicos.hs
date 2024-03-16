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
