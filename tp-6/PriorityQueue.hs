--1. Priority Queue (cola de prioridad)


--Ejercicio 1

{-La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
Implementarla usando listas, e indicando el costo de cada operación.-}

module PriorityQueue
  (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = PQ [a]

{-INV. REP.: en (PQ xs)
   -xs está ordenada de menor a mayor siendo el primer elemento el de mayor prioridad.
-}

emptyPQ :: PriorityQueue a --O(1)
--Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool --O(1)
--Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

-- ===============================================================================================

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --O(n) por el costo de insertarEnOrden
--Propósito: inserta un elemento en la priority queue.
insertPQ x (PQ xs) = PQ (insertarEnOrden x xs)

insertarEnOrden :: Ord a => a -> [a] -> [a] --O(n) porque hace recursión sobre la lista siendo n la longitud de la misma.
insertarEnOrden x [] = [x]
insertarEnOrden x (y:ys) = if x < y 
                           then x : y : ys
                           else y : insertarEnOrden x ys

-- ===============================================================================================

findMinPQ :: Ord a => PriorityQueue a -> a --O(1) 
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMinPQ (PQ xs) = head xs

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --O(1)
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ (PQ xs) = PQ (tail xs)




