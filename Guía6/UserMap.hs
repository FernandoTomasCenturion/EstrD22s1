import Map

value :: Maybe v -> v 
value Nothing  = error "no se puede obtener un value"
value (Just x) = x


valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = maybeValues (keys map) map 

maybeValues :: Eq k => [k] -> Map k v -> [Maybe v]
maybeValues [] m = []
maybeValues (k:ks) m = lookUpM k m : maybeValues ks m

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas []     map = True  
todasAsociadas (k:ks) map = case lookUpM k map of 
                            Just   v -> True && todasAsociadas ks map 
                            Nothing -> False 

mapToList :: Eq k => Map k v -> [(k, v)] 
mapToList map = mapaAList (keys map) map 

mapaAList ::  Eq k => [k] -> Map k v -> [(k, v)]
mapaAList []     map = []
mapaAList (k:ks) map =(k, value(lookUpM k map)) : mapaAList ks map

agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq kvs = agruparEq' kvs emptyM 

agruparEq' :: Eq k => [(k, v)] -> Map k [v] -> Map k [v] 
agruparEq' [] map          = map 
agruparEq' ((k,v):kvs) map = agruparKeyValueAMap k v (agruparEq' kvs map)

agruparKeyValueAMap :: Eq k => k -> v -> Map k [v] -> Map k [v]
agruparKeyValueAMap k v map = case lookUpM k map of 
                              Just  v'  -> assocM k (v:v') map 
                              Nothing   -> assocM k  [v]   map 

incrementar :: Eq k => [k] -> Map k Int -> Map k Int     
incrementar []     map = map
incrementar (k:ks) map = case lookUpM k map of 
                         Just v  -> assocM k (v+1) (incrementar ks (deleteM k map))
                         Nothing -> incrementar ks map      

mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = mergeMaps' map1 map2 (keys map1)

mergeMaps' :: Eq k => Map k v -> Map k v -> [k] -> Map k v
mergeMaps' map1 map2 []     = map2
mergeMaps' map1 map2 (x:xs) = case lookUpM x map1 of
                                Just v -> mergeMaps' map1 (assocMergeMap x v map2) xs
                                Nothing -> mergeMaps' map1 map2 xs

assocMergeMap :: Eq k => k -> v -> Map k v -> Map k v
assocMergeMap key value map = case lookUpM key map of
                                    Just v -> assocM key value (deleteM key map)
                                    Nothing -> assocM key value map
