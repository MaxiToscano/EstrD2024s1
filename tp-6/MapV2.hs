--Ejercicio 4

{-Implemente las siguientes variantes del tipo Map, indicando los costos obtenidos para cada operación, 
  justificando las respuestas:

2. Como una lista de pares-clave valor con claves repetidas
-}

module MapV2
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
  where

data Map k v = M [(k,v)]
--No posee invariante de representación ya que esta versión tiene claves repetidas.

--La interfaz del tipo abstracto Map es la siguiente:

emptyM :: Map k v --O(1)
--Propósito: devuelve un map vacío
emptyM = M []

-- ==============================================================================================

assocM :: Eq k => k -> v -> Map k v -> Map k v --O(1) 
--Propósito: agrega una asociación clave-valor al map. 
assocM k v (M kvs) = M ((k,v) : kvs) 

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

keys :: Eq => Map k v -> [k] --O(n^2) por el costo de clavesMSinRepetir.
--Propósito: devuelve las claves del map.
keys (M kvs) = clavesMSinRepetir kvs

clavesMSinRepetir :: Eq k => [(k,v)] -> [k] --O(n^2) porque hace recursión sobre la lista utiliando elem de costo lineal. n = longitud de la lista
clavesMSinRepetir [] = []
clavesMSinRepetir ((k,v):kvs) = if elem k (clavesMSinRepetir kvs)
                                then clavesMSinRepetir kvs
                                else k : (clavesMSinRepetir kvs)