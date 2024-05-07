--Ejercicio 4

{-Implemente las siguientes variantes del tipo Map, indicando los costos obtenidos para cada operación, 
  justificando las respuestas:

3. Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está
   asociada al valor en la misma posición, pero de la otra lista.
-}

module MapV3
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
  where

data Map k v = M [k] [v]
  {- INV.REP.: 
      En (M ks vs): 
      La clave ubicada en la posición i de ks está asociada al valor en la misma posición de vs.  
      La longitud de ks y vs es igual.
      No hay claves repetidas en ks
  -}

--La interfaz del tipo abstracto Map es la siguiente:

emptyM :: Map k v --O(1)
--Propósito: devuelve un map vacío
emptyM = M [] []

-- ==============================================================================================

assocM :: Eq k => k -> v -> Map k v -> Map k v --O(n) por el costo de asociar.
--Propósito: agrega una asociación clave-valor al map. 
assocM k v (M ks vs) = if elem k ks 
                       then M ks (cambiarValorEnPosicion (posDeClave k ks) v vs)
                       else M (k:ks) (v:vs)

cambiarValorEnPosicion :: Eq k => int -> v -> [v] -> [v]
cambiarValorEnPosicion 0 v (v':vs) = v:vs
cambiarValorEnPosicion n v (v':vs) = v' : cambiarValorEnPosicion (n-1) v vs

posDeClave :: Eq k => k -> [k] -> Int  
--PRECOND: la calve se encuentra en la lista         
posDeClave _ [] = 0
posDeClave k (k':ks) = unoSi (k != k') + posDeClave k ks

-- =============================================================================================

lookupM :: Eq k => k -> Map k v -> Maybe v --O(n) por el costo de buscar.
--Propósito: encuentra un valor dado una clave.
lookupM 

-- ==============================================================================================

deleteM :: Eq k => k -> Map k v -> Map k v --O(n) por el costo de borrarK
--Propósito: borra una asociación dada una clave.
deleteM 

-- ==============================================================================================

keys :: Map k v -> [k] --O(1) 
--Propósito: devuelve las claves del map.
keys (M ks _) = ks

