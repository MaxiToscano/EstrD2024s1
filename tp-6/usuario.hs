--Ejercicio 2

{-Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
menor a mayor utilizando una Priority Queue como estructura auxiliar. ¿Cuál es su costo?
OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority
Queues basada en una estructura concreta llamada Heap, que será trabajada en la siguiente práctica.-}

heapSort :: Ord a => [a] -> [a]
--dada una lista la ordena de menor a mayor utilizando una Priority Queue como estructura auxiliar
heapSort [] = []
heapSort xs = ordenarPQ (armarPQconLista xs)

armarPQconLista :: Ord a => [a] -> PriorityQueue a
armarPQconLista [] = emptyPQ
armarPQconLista (x:xs) = insertPQ x armarPQconLista xs

ordenarPQ :: Ord a => PriorityQueue a -> [a] 
ordenarPQ pq = findMinPQ pq : ordenarPQ (deleteMinPQ pq)

-- ///////////////////////////////////////////////////////////////////////////////////////////


--Implementar como usuario del tipo abstracto Map las siguientes funciones:


--1. 
valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = valoresK (keys M) m

valoresK :: [k] -> Map k v -> [Maybe v]
valoresK [] _ = []
valoresK (k:ks) m = lookupM k m : valoresK ks m

-- =========================================================================================  

--2. 
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas [] _ = False
todasAsociadas (k:ks) m = elem k (keys m) && todasAsociadas ks m


-- =========================================================================================  

--3. 
listToMap :: Eq k => [(k, v)] -> Map k v
--Propósito: convierte una lista de pares clave valor en un map.
listToMap [] = EmptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

-- =========================================================================================  

--4. 
mapToList :: Eq k => Map k v -> [(k, v)]
--Propósito: convierte un map en una lista de pares clave valor.
mapToList m = mapaALista (keys m) m

mapaALista :: [k] -> Map v -> [(k, v)]
mapaALista [] _ = []
mapaALista (k:ks) m = (k, fromJust (lookupM k m)) : mapaALista ks m
 
fromJust :: Maybe a -> a -- O(1)
-- PRECOND: No puede ser Nothing
fromJust (Just x) = x

-- =========================================================================================  
    
--5. COMPLETAR
agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq []        = EmptyM
agruparEq (kv:kvs)  = asociarConLista kv (agruparEq kvs)

asociarConLista :: Eq k => (k,v) -> Map k [v] -> Map k [v] 
asociarConLista (k,v) map = if esValor (lookupM k map) 
                            then assocM k (v:(valorDe k map)) map
                            else assocM k [v] map 

esValor :: Maybe v -> Bool
esValor Nothing  = False 
esValor (Just x) = True

-- =========================================================================================  

--6. 
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar ks m = incrementarK ks (keys m) m


incrementarK :: Eq k => [k] -> [k] -> Map k Int -> Map k Int
incrementarK [] _ m            = m
incrementarK _ [] m = m        = m
incrementarK (k:ks) (k':ks') m = if k==k'
                                 then assocM k' (fromJust (lookupM k' m + 1)) (incrementarK ks ks' m)
                                 else incrementarK ks ks' m

-- =========================================================================================

--7. 
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. 
--Si una clave del primero existe en el segundo, es reemplazada por la del primero.


--Indicar los ordenes de complejidad en peor caso de cada función implementada, justificando las respuestas.