module Set (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) 

where 

data Set a = ConsS [a] Int deriving Show

{-
Inv: La lista que esta dentro de la estructura del set no contiene elementos repetidos.
     El int dentro de la estructura guarda la cantidad de elementos dentro de la estructura.
-}

s1 :: Set Int 
s1 = ConsS [1,2,3,4,5] 5

s2 :: Set Int 
s2 = ConsS [7,8,9,1] 4

--O(1)
emptyS :: Set a 
emptyS = ConsS [] 0

pertenece :: Eq a => a -> [a] -> Bool 
pertenece x ys = elem x (ys)


--O(n)-- preguntar como justificar cuando se fija si un elemento esta dentro de una lista ,y si no esta lo agrega.
addS :: Eq a => a -> Set a -> Set a 
addS e (ConsS xs n) = if pertenece e xs 
                       then ConsS xs n
                       else ConsS (e:xs) n

--O(m) siendo m la cantidad de elementos de la lista del set.
belongs :: Eq a => a -> Set a -> Bool 
belongs x (ConsS xs n) = pertenece x xs

--Orden(n), siendo n la cantidad de elementos del conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (ConsS xs n) = length xs

--O(n) siendo n la cantidad de elementos de la lista del set dado.
removeS :: Eq a => a -> Set a -> Set a
removeS e (ConsS xs n) = ConsS (removeL e xs) n

--O(n) siendo n la cantidad de elementos de la lista
removeL :: Eq a => a -> [a] -> [a]
removeL e [] = []
removeL e (x:xs) =  if e == x
                        then xs 
                        else x : (removeL e xs)

--O(n*m), siendo n la cantidad de elementos del primer set y m la cantidad de elementos del segundo set.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs n1) (ConsS ys n2)  = let newList = unirListasSinRepetidos xs ys 
                                      in ConsS newList (length newList)

--O(n*m) siendo n la cantidad de elementos de la primera lista dada y m la cantidad de elementos de la segunda lista dada
unirListasSinRepetidos ::  Eq a => [a] -> [a] -> [a] 
unirListasSinRepetidos xs     [] = xs
unirListasSinRepetidos (x:xs) ys = agregarSiNoEsta x (unirListasSinRepetidos xs ys)

--O(n), siendo n la cantidad de elementos de la lista.
agregarSiNoEsta :: Eq a => a -> [a] -> [a] 
agregarSiNoEsta element []     = element : []
agregarSiNoEsta element (x:xs) = if element == x
                                 then (x:xs) 
                                 else x : agregarSiNoEsta element xs


--O(n²), siendo n la cantidad elementos que se agregaron al 
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs n) = sinRepetidos xs 