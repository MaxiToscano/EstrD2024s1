--                                  PRÁCTICA 3: TIPOS RECURSIVOS


--1. Tipos recursivos simples

--1.1. Celdas con bolitas
--Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:

data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia

{-En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
ser la siguiente:
Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
Implementar las siguientes funciones sobre celdas:
-}

nroBolitas :: Color -> Celda -> Int
--Dados un color y una celda, indica la cantidad de bolitas de ese color. 
--Nota: pensar si ya existe una operación sobre listas que ayude a resolver el problema.
nroBolitas _ CeldaVacia = 
nroBolitas c (Bolita c' ce) = unoSi c==c' + nroBolitas ce

{-poner :: Color -> Celda -> Celda
Dado un color y una celda, agrega una bolita de dicho color a la celda.
sacar :: Color -> Celda -> Celda
Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
Gobstones, esta función es total.
ponerN :: Int -> Color -> Celda -> Celda
Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.-}