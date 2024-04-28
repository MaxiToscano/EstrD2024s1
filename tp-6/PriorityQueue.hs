--1. Priority Queue (cola de prioridad)


--Ejercicio 1

{-La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
Implementarla usando listas, e indicando el costo de cada operación.-}

module PriorityQueue
  (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = PQ [a]

emptyPQ :: PriorityQueue a --O(1)
--Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool --O(1)
--Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --O(1)
--Propósito: inserta un elemento en la priority queue.
insertPQ (PQ xs) = PQ (x:xs)

findMinPQ :: Ord a => PriorityQueue a -> a --O(n) por el costo de minimum que hace recursión sobre la lista, siendo n ela longitud de la lista.
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMinPQ (PQ xs) = if null xs 
                    then error "Es EmptyPQ"
                    else minimun xs

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ (PQ xs) = quitarMinimo xs

quitarMinimo :: Ord a => [a] -> [a]
--PRECOND: no es una lista vacia.
quitarMinimo [] = error "es una lista vacía"
quitarMinimo (x:[]) = []
quitarMinimo (x:xs) = if x < minimum xs
                      then xs 
                      else x : quitarMinimo xs


