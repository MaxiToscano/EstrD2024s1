--2. Map (diccionario)

--Ejercicio 4
{-Implemente las siguientes variantes del tipo Map, indicando los costos obtenidos para cada operación, 
  justificando las respuestas:

1. Como una lista de pares-clave valor sin claves repetidas.-}

module MapV1
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
  where

data Map k v = M [(k,v)]
  {- INV.REP.: 
      en M kvs, no hay claves repetidas en kvs 
  -}

--La interfaz del tipo abstracto Map es la siguiente:

emptyM :: Map k v --O(1)
--Propósito: devuelve un map vacío
emptyM = M []

-- ==============================================================================================

assocM :: Eq k => k -> v -> Map k v -> Map k v --O(n) por el costo de asociar.
--Propósito: agrega una asociación clave-valor al map. 
assocM k v (M kvs) = M (asociar k v kvs) -- M ((k,v) : kvs) ASÍ NO, porque puede violar el Invariante!!


asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)] --O(n) porque hace recursión sobre la lista, siendo n la longitud de la lista.
asociar k v [] = [(k,v)]
asociar k v ((k',v'): kvs) = if k == k' 
                             then (k',v) : kvs
                             else (k',v') : asociar k v kvs

-- =============================================================================================

lookupM :: Eq k => k -> Map k v -> Maybe v --O(n) por el costo de buscar.
--Propósito: encuentra un valor dado una clave.
lookupM k (M kvs) = buscar k kvs 

buscar :: Eq k => k -> [(k,v)] -> Maybe v --O(n) porque hace recursión sobre la lista, siendo n la longitud de la lista.
buscar _ [] = Nothing
buscar k ((k',v):kvs) = if k==k'
                        then Just v
                        else buscar k kvs

-- ==============================================================================================

deleteM :: Eq k => k -> Map k v -> Map k v --O(n) por el costo de borrarK
--Propósito: borra una asociación dada una clave.
deleteM k (M kvs) = M (borrarK k kvs)

borrarK :: Eq k => k -> [(k, v)] -> [(k, v)] --O(n) porque hace recursión sobre la lista, siendo n la longitud de la lista.
borrarK k []             = []
borrarK k ((k', v'):kvs) = if k == k'
                           then borrarK k kvs
                           else (k', v') : borrarK k kvs

-- ==============================================================================================

keys :: Map k v -> [k] --O(n) por el costo de calvesM.
--Propósito: devuelve las claves del map.
keys (M kvs) = clavesM kvs

clavesM :: [(k,v)] -> [k] --O(n) porque hace recursión sobre la lista, siendo n la longitud de la lista.
clavesM [] = []
clavesM ((k,v):kvs) = k : clavesM kvs