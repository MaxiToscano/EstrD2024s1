--4. Stack (pila)

{-Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
platos). Su interfaz es la siguiente: emptyS, isEmptyS, push, top, pop, lenS
-}

--Implementar el tipo abstracto Stack utilizando una lista.

module Stack 
    (Stack, emptySt, isEmptySt, push, top, pop, lenS)

where

data Stack a = St [a] Int
    deriving Show

{- INV.REP: en (Set xs n)
    * n es la longitud de la lista
-}

--valido: St [1,2,2,3] 4
 

emptySt :: Stack a --O(1)
--Crea una pila vacía.
emptySt = St [] 0

isEmptySt :: Stack a -> Bool --O(1)
--Dada una pila indica si está vacía.
isEmptySt (St xs _) = null xs

push :: a -> Stack a -> Stack a --O(1)
--Dados un elemento y una pila, agrega el elemento a la pila.
push x (St xs n) = St (x:xs) (n+1)

top :: Stack a -> a --O(1)
--Dada un pila devuelve el elemento del tope de la pila.
--PRECOND: la pila no es EmptyS
top (St xs _) = head xs

pop :: Stack a -> Stack a --O(1)
--Dada una pila devuelve la pila sin el primer elemento.
pop (St xs n) = St (tail xs) (n-1)

lenS :: Stack a -> Int --O(1)
--Dada la cantidad de elementos en la pila.
--Costo: constante.
lenS (St _ n) = n



