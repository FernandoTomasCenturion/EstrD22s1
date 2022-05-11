module Set (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) 

where

data Set a = ConsS [a] 

{-
La inv. de representación habla de la representación del TAD.
-}

{-
En el caso de tener un Int en la representacion, n es la cantidad de elemetos DISTINTOS de xs.
-}

{-
Inv. Rep: No tiene.
-}
--O(m)
pertenece :: Eq a => a -> [a] -> Bool 
pertenece   a []     = False    
pertenece   a (x:xs) = a == x || pertenece a xs 

--O(n)
longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 

--O(n*m)
sinRepetidosMejor :: Eq a => [a] -> [a]
sinRepetidosMejor []     = []
sinRepetidosMejor (x:xs) = let xs' = sinRepetidosMejor xs
                                     in if pertenece x xs'
                                         then     xs'
                                         else x : xs' 


s1 :: Set Int
s1 = ConsS [1,2,3,4,5,5]

s2 :: Set Int
s2 = ConsS [7,8,9,1]

--O(1)
emptyS :: Set a 
emptyS = ConsS []

--O(1)
addS :: Eq a => a -> Set a -> Set a 
addS x (ConsS xs) = ConsS (x:xs)

--O(M), siendo M la cantidad de veces que se agregaron elementos al Set. 
belongs :: Eq a => a -> Set a -> Bool 
belongs x (ConsS xs) = pertenece x xs

--Orden(n), siendo n la cantidad de elementos del conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (ConsS xs) = longitud xs 

--O(n) siendo n la cantidad de elementos de la lista del set.
removeS :: Eq a => a -> Set a -> Set a
removeS e (ConsS xs) = ConsS (removeL e xs) 

--O(n) siendo n la cantidad de elementos de la lista
removeL :: Eq a => a -> [a] -> [a]
removeL e [] = []
removeL e (x:xs) =  if e == x
                        then xs 
                        else x : (removeL e xs)

--O(n*m) siendo n la cantidad de elementos de la lista del set 1 y m la cantidad de elementos de la lista del set2.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs) s  = agregarAlTodosSet xs s


agregarAlTodosSet :: Eq a => [a] -> Set a -> Set a
agregarAlTodosSet [] s     = s
agregarAlTodosSet (x:xs) s = addS x (agregarAlTodosSet xs s)

--O(1)
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs) = xs


instance Show a => Show (Set a) where 
    show (ConsS xs) = "{" ++ mostrar xs ++ "}"

mostrar []    = ""
mostrar [x]   = show x 
mostrar(x:xs) =  show x ++ "," ++ mostrar xs
