import Set 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

--O(n²) con set y setV2 siendo n la cantidad de elementos de la lista dada. 
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s =  if belongs x s 
                             then x : losQuePertenecen xs s
                             else losQuePertenecen xs s

--O(n²) con set y setv2, siendo n la cantidad de elementos de la lista dada.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos xs = setToList (toSetList xs)

--O(n)  con set, siendo n la cantidad de elementos de la lista dada,
--O(n²) con setV2, siendo n la cantidad de elementos de la lista dada.
toSetList :: Eq a => [a] -> Set a
toSetList []     = emptyS
toSetList (x:xs) = addS x (toSetList xs)

--O(n³) con setV2, siendo n la cantidad de elementos que posee el árbol dado. 
--O(n²) con set, siendo n la cantidad de elementos que posee el árbol dado.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos (EmptyT) = emptyS
unirTodos (NodeT s ti td) = unionS (unionS s (unirTodos ti)) (unirTodos td)                             