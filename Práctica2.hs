--Recursión sobre listas.

sumatoria :: [Int] -> Int 
sumatoria []      = 0
sumatoria (x:xs)  = x + sumatoria xs  

longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 


--- para sea el sucesor de cada nro, le tengo que sumar 1 a cada elemento de la lista
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) =  (x + 1) : sucesores xs 


conjuncion :: [Bool] -> Bool
conjuncion []      = error "No hay elementos en la lista"
conjuncion (b:[])  = b
conjuncion (x:xs)  = x && conjuncion xs 



disyuncion :: [Bool] -> Bool
disyuncion []      = error "No hay elementos en la lista"
disyuncion (b:[])  = b
disyuncion (x:xs)  = x || disyuncion xs 


pertenece :: Eq a => a -> [a] -> Bool 
pertenece   a []     = False    
pertenece   a (x:xs) = (a == x) || pertenece a xs 

---Por cada aparicion de "a" sumo 1.
apariciones :: Eq a => a -> [a] -> Int
apariciones a []     = 0
apariciones a (x:xs) = unoSi(a == x) + apariciones a xs
                        
unoSi:: Bool -> Int 
unoSi True     = 1
unoSi False    = 0 

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA  n []       = []
losMenoresA  n (x:xs)   =  if (esMayorA n x)     
                           then x : losMenoresA n xs    
                           else losMenoresA n xs


esMayorA :: Int -> Int -> Bool 
esMayorA n z=  n > z


agregarAlFinal :: [a] -> a -> [a] 
agregarAlFinal []     a  = [a]
agregarAlFinal (x:xs) a  = x : (agregarAlFinal xs a)
 

reversa :: [a] -> [a] 
reversa []     = []
reversa (x:xs) =  agregarAlFinal (reversa xs) x