module PriorityQueue (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ) where

data PriorityQueue a = ConsPQ [a] 

instance Show a => Show (PriorityQueue a) where 
    show (ConsPQ xs) = "{" ++ mostrar xs ++ "}"

mostrar []    = ""
mostrar [x]   = show x 
mostrar(x:xs) =  show x ++ "," ++ mostrar xs

pq1= ConsPQ [1,2,3]

--agregar :: [a] -> [a] -> [a] orden (n), siendo n la longitud de la lista.
--En heap sort se agregan las cosas y se sacan las cosas.
emptyPQ :: PriorityQueue a 
emptyPQ = ConsPQ []

isEmptyPQ :: PriorityQueue a -> Bool 
isEmptyPQ (ConsPQ xs) = null xs 

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a 
insertPQ e (ConsPQ xs) = ConsPQ(e:xs) 

findMinPQ :: Ord a => PriorityQueue a -> a 
--La Lista no está vacía.
findMinPQ (ConsPQ xs) = minimum xs

--Qué pasa con los elementos repetidos??
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (ConsPQ xs) = (ConsPQ (borrarMin xs))

borrarMin :: Ord a => [a] -> [a]
  -- PRECOND: la lista no es vacía
borrarMin xs = borrar (minimum xs) xs

-- O(n)
borrar :: Eq a => a -> [a] -> [a]
borrar x []     = []
borrar x (y:ys) = if x==y 
                  then ys 
                  else y : borrar x ys
