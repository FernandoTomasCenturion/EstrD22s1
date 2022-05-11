--Costo O(1)
head’ :: [a] -> a
head’ (x:xs) = x

--Costo O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

--Costo O(n), siendo n la cantidad de números.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--Costo 0(n), siendo n la cantidad de elemento
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs


--O(n*m), siendo n la longitud de la primera lista y m la suma de todos los números de la lista (en peor caso).
factoriales :: [Int] -> [Int]
factoriales []     = []
factoriales (x:xs) = factorial x : factoriales xs

--O(n)
pertenece :: Eq a => a -> [a] -> Bool
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs

--O(n²), siendo n la longitud de la lista. Sin repetidos usa pertenece, que es otra función de costo lineal.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if pertenece x xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs

--O(n), siendo n la longitud de elementos de la primer lista dada.
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys    = ys
append (x:xs) ys = x : append xs ys

--O(n), siendo n la longitud de la lista.
concatenar :: [String] -> String
concatenar []     = []
concatenar (x:xs) = x ++ concatenar xs

--O(n), siendo n el número dado por parámetro
takeN :: Int -> [a] -> [a]
takeN 0 xs     = []
takeN n []     = []
takeN n (x:xs) = x : takeN (n-1) xs

--O(n), siendo n el número dado por parámetro
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

--O(n), siendo n la longitud de la lista dada.
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

--O(n)
minimo :: Ord a => [a] -> a
minimo [x]    = x
minimo (x:xs) = min x (minimo xs)

--O(n)
sacar :: Eq a => a -> [a] -> [a]
sacar n []     = []
sacar n (x:xs) = if n == x 
                 then xs
                 else x : sacar n xs

--O(n²)
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs = let m = minimo xs in m : ordenar (sacar m xs)