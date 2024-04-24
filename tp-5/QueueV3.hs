--5. Queue con dos listas


{-Implemente la interfaz de Queue pero en lugar de una lista utilice dos listas. Esto permitirá
que todas las operaciones sean constantes (aunque alguna/s de forma amortizada).
La estructura funciona de la siguiente manera. Llamemos a una de las listas fs (front stack) y
a la otra bs (back stack). Quitaremos elementos a través de fs y agregaremos a través de bs, pero
todas las operaciones deben garantizar el siguiente invariante de representación: Si fs se encuentra
vacía, entonces la cola se encuentra vacía.
¿Qué ventaja tiene esta representación de Queue con respecto a la que usa una sola lista?-}

module QueueV3 
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)

where

data Queue = Q [a] [a]
--             fs  bs

{-
Invariante de representación: 

-Si fs se encuentra vacía, entonces la cola se encuentra vacía.

-}

--Valido -> Q [1,2,3] [4,5,6]
--invalido -> Q [] [4,5,6]

emptyQ :: Queue a --O(1)
--Crea una cola vacía.
emptyQ = Q [] []

isEmptyQ :: Queue a -> Bool --O(1)
--Dada una cola indica si la cola está vacía.
isEmptyQ (Q fs bs) = null fs

enqueue :: a -> Queue a -> Queue a 
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q fs bs) = 

firstQ :: Queue a -> a 
--Dada una cola devuelve el primer elemento de la cola.
--PRECOND: la cola no es Empty
firstQ (Q fs bs) = 

dequeue :: Queue a -> Queue a 
--Dada una cola la devuelve sin su primer elemento.
--PRECOND: la cola no es Empty
dequeue (Q fs bs) = 