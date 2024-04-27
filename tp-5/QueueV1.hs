--3. Queue (cola)

{-Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa
que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
primero en salir (como la cola de un banco). 
Su interfaz es la siguiente:  (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)


1. Implemente el tipo abstracto Queue utilizando listas. 
   Los elementos deben encolarse por el final de la lista y desencolarse por delante.
-}

module QueueV1 
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)

where

data Queue a = Q [a]

--No tiene invariantes de representación.

emptyQ :: Queue a --O(1)
--Crea una cola vacía.
emptyQ = Q []

isEmptyQ :: Queue a -> Bool --O(1)
--Dada una cola indica si la cola está vacía.
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a --O(n) por el costo lineal de ++
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q xs) = Q (xs ++ [x])

firstQ :: Queue a -> a --O(1)
--Dada una cola devuelve el primer elemento de la cola.
--PRECOND: la cola no es Empty
firstQ (Q xs) = head xs

dequeue :: Queue a -> Queue a --O(1)
--Dada una cola la devuelve sin su primer elemento.
--PRECOND: la cola no es Empty
dequeue (Q xs) = Q (tail xs)



