module RAList (RAList) where 

import Map 
import Heap

data RAList a = MkR Int (Map Int a) (Heap a)

{-Inv. Rep: Con la raList = siendo MkR n m h, 
            *n >= 0.
            *lenght (domM m) = n. O sea, la longitud del map debe ser igual a n. 
            *Todos los elementos de m deben estar en h y viceversa.
-}

emptyRAL :: RAList a 
--Eficiencia: O(1)
emptyRAL = 0 emptyM emptyH 


isEmptyRAL :: RAList a -> Bool
--Eficiencia: O(1)
isEmptyRAL (MkR n m h) = isEmptyH h 

lengthRAL :: RAList a -> Int
--Eficiencia: O(1)
lenghtRAL (MkR n m h) = n

get :: Int -> RAList a -> a
--Eficiencia: O(Log N)
get x (MkR n m h) = fromJust (lookUpM x m) 
--Justificación: *Costo de lookUpM O(Log N) 

minRAL :: Ord a => RAList a -> a
--Eficiencia: O(1)
minRAL (MkR n m h) = findMin h 

add :: Ord a => a -> RAList a -> RAList a
--Eficiencia: O(Log N)
add x (MkR n m h) = MkR (n+1) 
                        (agregarElementoEnMap x n m) 
                        (agregarElementoEnHeap x h) 

agregarElementoEnMap :: a -> Int -> Map Int a -> Map Int a 
--Precondición: El int dado no existe en el map dado
--Eficiencia: O(Log N)
agregarElementoEnMap x n m= case lookUpM n m of 
                            Nothing -> assocM x n m 
                            Just v  -> error "No se cumple la precondición" 
--Justificación: 
--               *Costo de assocM  O(Log N) 
--               *Costo de lookUpM O(Log N)
--               *Costo Final      O(Log N)

agregarElementoEnHeap :: Ord a => a -> Heap a -> Heap a 
agregarElementoEnHeap  x h = insertH x (agregarElementoEnHeap x h)
--Justificación: 
--              *Costo de insertH O(Log N)


elems :: Ord a => RAList a -> [a]
--Eficiencia: O (N Log N)
elems (MkR n m h) = elemsDeHeap h 

elemsDeHeap :: Ord a => Heap a -> [a]
--Eficiencia: O(N Log N)
elemsDeHeap h = if isEmptyH 
                then []
                else findMin h : (elemsDeHeap (deleteMin h))
--Justificación: 
--              *Costo de findMin   O(1)
--              *Costo de isEmptyH  O(1)
--              *Costo de deleteMin O(Log N)
--Esta operación se hace por cada elemento que hay en la heap.

remove :: Ord a => RAList a -> RAList a
--Eficiencia: O(Log N)
remove (MkR n m h) = MkR (n-1) (deleteM (n-1) m) (modificarH h) 

modificarH :: Ord a => a -> Heap a -> Heap a 
--Eficiencia: O(Log N)
modificarH  x h = if isEmptyH heap 
                   then emptyH 
                   else if x == findMin h  
                   then modificarH x (deleteMin h)
                   else insertH (findMin h) (modificarH x (deleteMin h))

--Justificación: 
--               *Costo de isEmptyH   O(1) 
--               *Costo de emptyH     O(1)
--               *Costo de findMin    O(1)
--               *Costo de deleteMin  O(Log N) 
--               *Costo de insertH    O(Log N)




set :: Ord a => Int -> a -> RAList a -> RAList a
--Eficiencia: O(Log N)
set y x (MkR n m h) = MkR n (assocM y x m) (actualizarH fromJust(lookUpM y m)) x h)

actualizarH :: Ord a => a -> a -> a -> Heap a -> Heap a 
--Eficiencia: O(Log N)
actualizarH  x y z h = if x == findMin h 
                       then insertH z (deleteMin h) 
                       else insertH (findMin h) (actualizarH x y z (deleteMin h)) 

--Justificación: 
--               *Costo de findMin    O(1)
--               *Costo de deleteMin  O(Log N) 
--               *Costo de insertH    O(Log N)


addAt :: Ord a => Int -> a -> RAList a -> RAList a
--Eficiencia: O(Log N)
addAt x y (MkR n m h) = MkR (n+1) (desplazar x y n m) (insertH y h) 

desplazar :: a -> Int -> Int -> Map Int a 
--Eficiencia: O(Log N)
desplazar  x y n m = if y == n 
                     then assocM y x m
                     else case lookUpM (n-1) m of 
                          (Just x') -> assocM n x' (desplazar x y (n-1) m) 
                           Nothing  -> error "No se cumple la precondición"


--Justificación: 
--               *Costo de assocM     O(Log N)
--               *Costo de lookUpM    O(Log N)
