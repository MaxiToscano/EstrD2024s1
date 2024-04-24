
--PUNTO 2. Set (conjunto)

--Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

--emptyS, addS, belongs, sizeS, removeS, unionS, setToList



--1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda la cantidad de elementos en la estructura.
{-Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
de esta implementación, pero para mantener una interfaz común entre distintas posibles
implementaciones estamos obligados a escribir así los tipos.-}

module SetV1
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a] Int
--                 cant de elementos

{- INV.REP: en (Set xs n)
    * no hay elementos repetidos en xs
    * n es la longitud de la lista
-}

--valido -> [1,2,3,4] 4
--invalido -> [1,1,2,3] 3



emptyS :: Set a -- O(1)
--Crea un conjunto vacío.
emptyS = S [] 0 


addS :: Eq a => a -> Set a -> Set a -- O(n) -> siendo n el costo operaconal de elem
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs n) = if elem x xs 
                  then S xs n
                  else S (x:xs) (n+1)


belongs :: Eq a => a -> Set a -> Bool --O(n) -> siendo n el costo de elem
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs _) = elem x xs 


sizeS :: Eq a => Set a -> Int --O(1)
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S _ n) = n


-- ============================================================================
removeS :: Eq a => a -> Set a -> Set a --O(n) -> siendo n el costo de removerElem
--Borra un elemento del conjunto.
removeS x (S xs n) = S (removerElem x xs) (n-1)

--subtarea
removerElem :: Eq a => a -> [a] -> [a] --O(n) -> siendo n el tamaño de la lista dada.
--PRECOND: el elemento se encuentra en la lista.
removerElem _ [] = error "No se encuentra el elemento"
removerElem e (x:xs) = if e == x 
                       then xs 
                       else x : removerElem x xs


-- ================================================================================
unionS :: Eq a => Set a -> Set a -> Set a --O(n^2) -> por el costo operacional de addSS
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS (S xs n) s2 = addSS xs s2

--subtarea 
addSS :: Eq a => [a] -> Set a -> Set a --O(n^2) -> ta que utiliza addS de costo O(n) por cada elemento de la lista
addSS [] s = s
addSS (x:xs) s = addSS xs (addS x s)
-- ====================================================================================


setToList :: Eq a => Set a -> [a] --O(1)
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs _) = xs

-- ///////////////////////////////////////////////////////////////////////////////////////////


