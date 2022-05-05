import Set 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

--O(n) con set.hs
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s =  if belongs x s 
                             then x : losQuePertenecen xs s
                             else losQuePertenecen xs s

--O(n) con set.hs
--O(n*m) con setv2.hs
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos xs = setToList (toSetList xs)

toSetList :: Eq a => [a] -> Set a
toSetList []     = emptyS
toSetList (x:xs) = addS x (toSetList xs)

--O(n*m) con set.hs
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos (EmptyT) = emptyS
unirTodos (NodeT s ti td) = unionS (unionS s (unirTodos ti)) (unirTodos td)                             