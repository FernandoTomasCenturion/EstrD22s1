module Map (Map, emptyM, isEmptyM, assocM, lookUpM, deleteM, keys, m1, m2) where

data Map k v= M[(k, v)]

{-
Inv. Representación: Ninguna. 
-}

--Justificar costos.
m1 = M [("Tomas", 1000), ("Fernando", 2003), ("Perla", 2008)]
m2 = M [("Juan", 1), ("Pepito", 2)]


instance (Show k, Show v) => Show (Map k v) where
    show (M kvs) = "{\n " ++ mostrar kvs ++ "\n}"

mostrar []       = ""
mostrar [(k,v)]    = showKV (k,v) 
mostrar((k, v):kvs) =  showKV (k,v) ++ "\n," ++ mostrar kvs

showKV (k,v) = " " ++ show k ++ "->" ++ show v

--Orden(1), porque se crea el map.
emptyM :: Map k v 
emptyM = M []

--Orden(n), siendo n la cantidad de elementos de la lista.
isEmptyM :: Map k v -> Bool 
isEmptyM (M kvs) = null kvs

--Orden(n), siendo n la cantidad de elementos de la lista.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M kvs) = M (asociar k v kvs)

asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v []            = [ (k,v) ]
asociar k v ((k',v'):kvs) = if k==k' 
                            then (k',v):kvs 
                            else (k', v') : asociar k v kvs

lookUpM :: Eq k => k -> Map k v -> Maybe v
lookUpM key (M kvs) = buscar key kvs





buscar :: Eq k => k -> [(k,v)] -> Maybe v
buscar k []            = Nothing
buscar k ((k',v'):kvs) = if k==k' 
                        then Just v' 
                        else buscar k kvs    

deleteM :: Eq k => k -> Map k v -> Map k v
deleteM key (M kvs) = M (borrar key kvs)

borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
borrar k []            = []
borrar k ((k',v'):kvs) = if k==k' 
                         then kvs 
                         else (k',v') : borrar k kvs    

keys :: Map k v -> [k]
keys (M kvs) = claves kvs 

claves :: [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) = k : claves kvs
