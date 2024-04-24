--3. 
{-Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos, por ejemplo). 
Contrastar la eficiencia obtenida en esta implementación con la anterior.-}

module SetV2 
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a] 
               
-- No posee invariante de representación.


emptyS :: Set a -- O(1)
--Crea un conjunto vacío.
emptyS = S [] 


addS :: Eq a => a -> Set a -> Set a -- O(1)
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs) = S (x:xs) 


belongs :: Eq a => a -> Set a -> Bool --O(n) -> por el costo operacional de elem
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs) = elem x xs 

-- ===================================================================================

sizeS :: Eq a => Set a -> Int --O(n^2) -> por el costo operacional de listaSinRepetidos.
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs) = length (listaSinRepetidos xs)

--subtarea
listaSinRepetidos :: Eq a => [a] -> [a] --O(n^2) -> porque utiliza elem de costo O(n) por cada elemento de la lista
listaSinRepetidos [] = []
listaSinRepetidos (x:xs) = if elem x xs 
                           then listaSinRepetidos xs
                           else x : listaSinRepetidos xs
-- ==================================================================================

removeS :: Eq a => a -> Set a -> Set a --O(n) por el costo de removerElem.
--Borra un elemento del conjunto.
removeS x (S xs) = S (removerElem x xs) 

--subtarea
removerElem :: Eq a => a -> [a] -> [a] --O(n) porque utiliza == en cada elemento de la lista, siendo n el tamaño de la lista.
--PRECOND: el elemento se encuentra en la lista.
removerElem _ [] = error "No se encuentra el elemento"
removerElem e (x:xs) = if e == x 
                       then xs 
                       else x : removerElem x xs

-- ================================================================================

unionS :: Eq a => Set a -> Set a -> Set a --O(n^2) -> por el costo de listaSinRepetidos
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS (S xs) (S ys) = S (listaSinRepetidos (xs ++ ys))



setToList :: Eq a => Set a -> [a] --O(n^2) -> por el costo de listaSinRepetidos
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs) = listaSinRepetidos xs