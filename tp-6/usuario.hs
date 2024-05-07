import PriorityQueue
import MapV1


--Ejercicio 2

{-Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
menor a mayor utilizando una Priority Queue como estructura auxiliar. ¿Cuál es su costo?
OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority
Queues basada en una estructura concreta llamada Heap, que será trabajada en la siguiente práctica.-}


heapSort :: Ord a => [a] -> [a] --O(n^2) por el costo de listaAPQ
--Propósito: dada una lista la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar.
heapSort xs = pqALista (listaAPQ xs)


listaAPQ :: Ord a => [a] -> PriorityQueue a --O(n^2) porque hace recursión sobre la lista utilizando inserPQ que es de costo lineal.
--Propósito: dada una lista de elementos, devuelve una PriorityQueue con los elementos de la lista dada.
listaAPQ []     = emptyPQ
listaAPQ (x:xs) = insertPQ x (listaAPQ xs)


pqALista :: Ord a => PriorityQueue a -> [a]  --O(n) porque hace recursión sobre la PQ usando operaciones de costo constante
--Propósito: dada una Priority Queue retorna una lista con los elementos de la PQ, respetando el orden.
pqALista pq = if (isEmptyPQ pq)
              then []
              else findMinPQ pq : (pqALista (deleteMinPQ pq)) 

-- ///////////////////////////////////////////////////////////////////////////////////////////


--Implementar como usuario del tipo abstracto Map las siguientes funciones:


--1. 
valuesM :: Eq k => Map k v -> [Maybe v] --O(n^2) por el costo de valoresK
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = valoresK (keys m) m

valoresK :: Eq k => [k] -> Map k v -> [Maybe v] --O(n^2) porque hace recursión sobre la lista utilizando lookupM de costo lineal. n = longitud de la lista.
valoresK [] _ = []
valoresK (k:ks) m = lookupM k m : valoresK ks m

-- =========================================================================================  

--2. 
todasAsociadas :: Eq k => [k] -> Map k v -> Bool --O(n^2) porque hace recursión sobre la lista utilzando elem de costo lineal. n = longitud de la lista.
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas [] _ = False
todasAsociadas (k:ks) m = elem k (keys m) && todasAsociadas ks m


-- =========================================================================================  

--3. 
listToMap :: Eq k => [(k, v)] -> Map k v --O(n^2) porque hace recursión sobre la lista utilzando assocM de costo lineal. n = longitud de la lista.
--Propósito: convierte una lista de pares clave valor en un map.
listToMap [] = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

-- =========================================================================================  

--4. 
mapToList :: Eq k => Map k v -> [(k, v)] --O(n^2) por el costo de mapALista
--Propósito: convierte un map en una lista de pares clave valor.
mapToList m = mapALista (keys m) m

mapALista :: Eq k => [k] -> Map k v -> [(k, v)] --O(n^2) porque hace recursión sobre la lista utilzando lookupM de costo lineal. n = longitud de la lista.
mapALista [] _ = []
mapALista (k:ks) m = (k, fromJust (lookupM k m)) : mapALista ks m
 
fromJust :: Maybe a -> a -- O(1)
-- PRECOND: No puede ser Nothing
fromJust (Just x) = x

-- =========================================================================================  
    
--5. 
agruparEq :: Eq k => [(k, v)] -> Map k [v] --O(n^2) porque hace recursión sobre la lista utilizando asociarConLista de costo lineal. n = longitud de la lista.
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq []        = emptyM
agruparEq (kv:kvs)  = asociarConLista kv (agruparEq kvs)

asociarConLista :: Eq k => (k,v) -> Map k [v] -> Map k [v] --O(n) por el costo de lookupM y assocM
asociarConLista (k,v) m = if esValor (lookupM k m) 
                          then assocM k (v : (fromJust (lookupM k m))) m
                          else assocM k [v] m

esValor :: Maybe v -> Bool --O(1)
esValor Nothing  = False 
esValor (Just x) = True

-- =========================================================================================  

--6. 
incrementar :: Eq k => [k] -> Map k Int -> Map k Int --O(n^2) por el costo de incrementarK
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar ks m = incrementarK ks (keys m) m


incrementarK :: Eq k => [k] -> [k] -> Map k Int -> Map k Int --O(n^2) porque hace recursión sobre la lista utilizando assocM y lookupM de costo lineal. n = longitud de la lista.
incrementarK [] _ m            = m
incrementarK _ [] m            = m
incrementarK (k:ks) (k':ks') m = if k==k'
                                 then assocM k' (fromJust (lookupM k' m) + 1) (incrementarK ks ks' m)
                                 else incrementarK ks ks' m

-- =========================================================================================

--7. 
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v --O(n^2) por el costo de utilizar asociarKVs y mapToList
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. 
--Si una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps m1 m2 = asociarKVs (mapToList m1) m2

asociarKVs :: Eq k => [(k,v)] -> Map k v -> Map k v --O(n^2) porque hace recursión sobre la lista utilizando assocM de costo lineal. n = longitud de la lista.
asociarKVs [] m = m
asociarKVs ((k,v):kvs) m = assocM k v (asociarKVs kvs m)


-- ===========================================================================================

--Indicar los ordenes de complejidad en peor caso de cada función implementada, justificando las respuestas.