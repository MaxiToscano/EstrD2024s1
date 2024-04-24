--2. Como usuario del tipo abstracto Set implementar las siguientes funciones:

import SetV1
--import SetV2
import QueueV1
--import QueueV2
import Stack

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)


losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) s = if belongs x s 
                            then x : losQuePertenecen xs s
                            else losQuePertenecen xs s

-- =============================================================================================

sinRepetidos :: Eq a => [a] -> [a]
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos xs = setToList (sinRepetidos' xs)
 
sinRepetidos' :: Eq a => [a] -> Set a
sinRepetidos' [] = emptyS 
sinRepetidos' (x:xs) = addS x (sinRepetidos' xs)

-- ==============================================================================================

unirTodos :: Eq a => Tree (Set a) -> Set a
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.-}
unirTodos EmptyT = emptyS
unirTodos (NodeT s ti td) = unionS s (unionS (unirTodos ti) (unirTodos td))


-- ////////////////////////////////////////////////////////////////////////////////////////////


--3. Como usuario del tipo abstracto Queue implementar las siguientes funciones:


q = enqueue 4 (enqueue 3 (enqueue 2 (enqueue 1 emptyQ)))
q2 = enqueue 4 (enqueue 3 (enqueue 2 (enqueue 1 emptyQ)))

lengthQ :: Queue a -> Int
--Cuenta la cantidad de elementos de la cola.
lengthQ q = if isEmptyQ q 
            then 0
            else 1 + lengthQ (dequeue q)


queueToList :: Queue a -> [a]
--Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
queueToList q = if isEmptyQ q 
                then []  
                else firstQ q : queueToList (dequeue q)


unionQ :: Queue a -> Queue a -> Queue a
--Inserta todos los elementos de la segunda cola en la primera.
unionQ q1 q2 = if isEmptyQ q2 
               then q1 
               else enqueue (firstQ q2) (unionQ q1 (dequeue q2))


-- //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


--1. Como usuario del tipo abstracto Stack implementar las siguientes funciones:

st = push 5 (push 4 (push 3 emptySt))

apilar :: [a] -> Stack a
--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar [] = emptySt
apilar (x:xs) = push x (apilar xs) 


desapilar :: Stack a -> [a]
--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar st = if isEmptySt st
               then []
               else top st : desapilar (pop st)
    

insertarEnPos :: Int -> a -> Stack a -> Stack a
{-Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).-}
insertarEnPos 0 x st = push x st
insertarEnPos n x st = push (top s) (insertarEnPos (n-1) x (pop s))


