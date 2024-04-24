--2. Implemente ahora la versión que agrega por delante y quita por el final de la lista. 
--   Compare la eficiencia entre ambas implementaciones.

module QueueV2
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

enqueue :: a -> Queue a -> Queue a --O(1)
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q xs) = Q (x:xs) 

firstQ :: Queue a -> a --O(n) -> por el costo de ultimoElemento
--Dada una cola devuelve el primer elemento de la cola.
--PRECOND: la cola no es Empty
firstQ (Q xs) = ultimoElemento xs

--subtarea
ultimoElemento :: [a] -> a --O(n) porque hace recursión sobre la lista, siendo n la longitud de la lista
--PRECOND: la lista no es vacia.
ultimoElemento [] = error "Es una lista vacía"
ultimoElemento (x:[]) = x
ultimoElemento (x:xs) = ultimoElemento xs


dequeue :: Queue a -> Queue a --O(n) -> por el costo de sinUltimoElemento
--Dada una cola la devuelve sin su primer elemento.
--PRECOND: la cola no es Empty
dequeue (Q xs) = Q (sinUltimoElemento xs)

--subtarea
sinUltimoElemento :: [a] -> [a] --O(n) porque hace recursión sobre la lista, siendo n la longitud de la lista
--PRECOND: la lista no es vacia.
sinUltimoElemento [] = error "Es una lista vacía"
sinUltimoElemento (x:[]) = []
sinUltimoElemento (x:xs) = x : sinUltimoElemento xs
 