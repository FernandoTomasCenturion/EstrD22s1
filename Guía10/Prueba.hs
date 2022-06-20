singularSi:: a -> Bool -> [a] 
singularSi x True  = [x]
singularSi x False = []

esPar :: Int -> Bool 
esPar x = x `mod` 2 == 0


soloLosPares :: [Int] -> [Int]
soloLosPares []     = []
soloLosPares (x:xs) = if esPar x 
                      then x : soloLosPares xs 
                      else soloLosPares xs

soloLosParesConSS :: [Int] -> [Int] 
soloLosParesConSS [] = [] 
soloLosParesConSS (x:xs) = singularSi x (esPar x) ++ soloLosParesConSS xs                     


soloLosImparesConSS :: [Int] -> [Int] 
soloLosImparesConSS [] = [] 
soloLosImparesConSS (x:xs) = singularSi x (esImpar x) ++ soloLosImparesConSS xs 


pertenece :: Eq a => a -> [a] -> Bool
pertenece x []     = False
pertenece x (y:ys) = (x == y) || pertenece x ys


sinDuplicados :: Eq a => [a] -> [a]
sinDuplicados []     = []
sinDuplicados (x:xs) = singularSi x (not (pertenece x xs)) ++ sinDuplicados xs 


divisores :: [Int] -> [Int] 
divisores []     = []
divisores (x:xs) = (x `div` 2) : divisores xs 


esImpar :: Int -> Bool 
esImpar x = x `mod` 2 == 1


esDivisorDe3 :: Int -> Bool 
esDivisorDe3 x = x `mod` 3 == 0

soloLosDivisoresDe3 :: [Int] -> [Int] 
soloLosDivisoresDe3 []     = [] 
soloLosDivisoresDe3 (x:xs) = singularSi x (esDivisorDe3 x) ++ soloLosDivisoresDe3 xs

soloLosImpares :: [Int] -> [Int] 
soloLosImpares []     = []
soloLosImpares (x:xs) = if esImpar x 
                        then x : soloLosImpares xs
                        else soloLosImpares xs

--Otros ejemplos de recursión estructural.
borrar :: Eq a => a -> [a] -> [a]
borrar x []      = []
borrar x (y:ys)  = if x == y 
                   then borrar x ys 
                   else y : borrar x ys

borrar2 :: Eq a => a -> a -> [a] -> [a] 
borrar2 x y (t:ts) = borrar x (borrar y ts)

borrar3 :: Eq a => a -> a -> a -> [a] -> [a]
borrar3 x y s (t:ts) = borrar x (borrar y (borrar s ts))
