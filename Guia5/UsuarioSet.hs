import Set 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)


losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s =  if belongs x s 
                             then x : losQuePertenecen xs s
                             else losQuePertenecen xs s


sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos x = setToList (addA x emptyS)


addA :: Eq a => [a] -> Set a -> Set a
addA [] set = set
addA (x:xs) set = addS x (addA xs set)


unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos (EmptyT) = emptyS
unirTodos (NodeT s ti td) = unionS (unionS s (unirTodos ti)) (unirTodos td)                             