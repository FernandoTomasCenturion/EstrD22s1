module Set (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) 

where

data Set a = ConsS [a] 


pertenece :: Eq a => a -> [a] -> Bool 
pertenece   a []     = False    
pertenece   a (x:xs) = a == x || pertenece a xs 


longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 

sinRepetidosMejor :: Eq a => [a] -> [a]
sinRepetidosMejor []     = []
sinRepetidosMejor (x:xs) = let xs' = sinRepetidosMejor xs
                                     in if pertenece x xs'
                                         then     xs'
                                         else x : xs' 


s1 :: Set Int
s1 = ConsS [1,2,3,4,5,6,4]

s2 :: Set Int
s2 = ConsS [7,8,9,9]

emptyS :: Set a 
emptyS = ConsS []

addS :: Eq a => a -> Set a -> Set a 
addS x (ConsS xs) = ConsS (x:xs)

belongs :: Eq a => a -> Set a -> Bool 
belongs x (ConsS xs) = pertenece x xs

sizeS :: Eq a => Set a -> Int
sizeS (ConsS xs) = contarSinRepetidos xs 

contarSinRepetidos :: Eq a => [a] -> Int 
contarSinRepetidos []     = 0
contarSinRepetidos (x:xs) = if pertenece x xs 
                            then contarSinRepetidos xs 
                            else 1 + contarSinRepetidos xs 

removeS :: Eq a => a -> Set a -> Set a
removeS e (ConsS xs) = ConsS (removeL e xs) 

removeL :: Eq a => a -> [a] -> [a]
removeL e [] = []
removeL e (x:xs) =  if e == x 
                        then xs 
                        else x : (removeL e xs)

unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs) (ConsS ys) = ConsS (xs ++ ys)

setToList :: Eq a => Set a -> [a]
setToList (ConsS xs) = sinRepetidosMejor xs                   

instance Show a => Show (Set a) where 
    show (ConsS xs) = "{" ++ mostrar xs ++ "}"

mostrar []    = ""
mostrar [x]   = show x 
mostrar(x:xs) =  show x ++ "," ++ mostrar xs