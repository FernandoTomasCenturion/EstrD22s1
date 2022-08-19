

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


longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

borrarNVeces :: [a] -> Int -> [a] 
--Precondición: n debe ser menor o igual a la longitud de la lista dada.
borrarNVeces xs     0 = xs
borrarNVeces (x:xs) n = if n <= longitud xs  
                        then borrarNVeces xs (n-1) 
                        else (x:xs)
--Costo O(N) siendo n la cantidad de elementos de la lista dada.


data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show 

data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show 


barril0 = Comida
barril1 = Oxigeno
barril2 = Torpedo

componente2= LanzaTorpedos

componente1 = Almacen [barril0, barril1, barril2]


barrilesDeComponente :: Componente -> [Barril] 
barrilesDeComponente (Almacen barrilesC) = barrilesC
barrilesDeComponente _                   = []

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


productoria :: [Int] -> Int 
productoria []     = 1
productoria (x:xs) = x * productoria xs  

sucesores :: [Int] -> [Int] 
sucesores []     = [] 
sucesores (x:xs) = (x+1) : sucesores xs

algoritmoDeEuclides :: Integer -> Integer -> Integer 
algoritmoDeEuclides a 0 = a
algoritmoDeEuclides a b = algoritmoDeEuclides b (a `mod` b) 

factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]   
             

esPrimo :: Int -> Bool 
esPrimo n = factores n == [1,n]

numerosPrimos :: [Int] -> [Int] 
numerosPrimos []     = []
numerosPrimos (x:xs) = singularSi x (esPrimo x) ++ numerosPrimos xs  

numerosCoprimos :: [Int] -> [Int] 
numerosCoprimos []     = []
numerosCoprimos (x:xs) = singularSi x (not(esPrimo x)) ++ numerosCoprimos xs 

