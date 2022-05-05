module Set (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) 

where 

data Set a = ConsS [a] 

{-
Inv: La lista que esta dentro de la estructura del set no contiene elementos repetidos.
-}

----Ejemplos para probar de sets sin repetidos.
s1 :: Set Int
s1 = ConsS [1,2,3,4,5]

s2 :: Set Int
s2 = ConsS [7,8,9,1]

--Funciones auxiliares
--O(n*m), siendo n la cantidad de elementos y siendo m el costo que hereda de la función Pertenece
sinRepetidosMejor :: Eq a => [a] -> [a]
sinRepetidosMejor []     = []
sinRepetidosMejor (x:xs) = let xs' = sinRepetidosMejor xs
                                     in if pertenece x xs'
                                         then     xs'
                                         else x : xs' 

--O(m), siendo m la cantidad de elementos de la lista.
pertenece :: Eq a => a -> [a] -> Bool 
pertenece   a []     = False    
pertenece   a (x:xs) = a == x || pertenece a xs 

--O(n), siendo n la cantidad de elementos de la lista.
longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 



instance Show a => Show (Set a) where 
    show (ConsS xs) = "{" ++ mostrar xs ++ "}"

mostrar []    = ""
mostrar [x]   = show x 
mostrar(x:xs) =  show x ++ "," ++ mostrar xs

--O(1)
emptyS :: Set a 
emptyS = ConsS [] 


--O(m*n) siendo m el costo de la función pertenece y n siendo la longitud de la lista del set.
addS :: Eq a => a -> Set a -> Set a 
addS e (ConsS xs) = if pertenece e xs 
                       then ConsS xs
                       else ConsS (e:xs)

--O(m) siendo m la cantidad de elementos de la lista del set.
belongs :: Eq a => a -> Set a -> Bool 
belongs x (ConsS xs) = pertenece x xs

--Orden(n), siendo n la cantidad de elementos del conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (ConsS xs) = longitud xs 

--O(n) siendo n la cantidad de elementos de la lista del set dado.
removeS :: Eq a => a -> Set a -> Set a
removeS e (ConsS xs) = ConsS (removeL e xs) 

--O(n) siendo n la cantidad de elementos de la lista
removeL :: Eq a => a -> [a] -> [a]
removeL e [] = []
removeL e (x:xs) =  if e == x
                        then xs 
                        else x : (removeL e xs)

--O(n*m) siendo n la cantidad de elementos de la lista del set1 y m la cantidad de elementos de la lista del set2.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs) s2  = agregarAlTodosSet xs s2

--O(n) siendo n la cantidad de elementos de la lista que
agregarAlTodosSet :: Eq a => [a] -> Set a -> Set a
agregarAlTodosSet [] set     = set
agregarAlTodosSet (x:xs) set = addS x (agregarAlTodosSet xs set)

--O(1)
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs) = xs