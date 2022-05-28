module RAList (RAList,) where 

import Map 
import Heap 

data RAList a = MkR Int (Map Int a) (Heap a)
{-
Inv. Representación:  
                   *Los valores que están en el map también están en la heap y vicerversa. 
                   *La siguiente posición se representa con un número entero y debe ser >= 0.
                   *La siguiente posicion coincide con size del map y el size de la heap.
                   *No puede haber claves mayores o iguales a "siguiente posición". 
-}

--O(1)
emptyRAL :: RAList a
emptyRAL = MkR 0 emptyM emptyH

--O(1)
isEmptyRal :: RAList a -> Bool 
isEmptyM (MkR i mpI heap) = i == 0

--O(1)
lengthRAL :: RAList a -> Int
lengthRAL (MkR i mpi heap) = i

--O(Log n)
get :: Int -> RAList a -> a
get n (MkR i mpI) = case lookupM mpi n of 
                    (Just i) -> i 
                    Nothing  -> error "No cumple con la precondicion"

--O(1)
minRAL :: Ord a => RAList a -> a
minRAL  (MkR i mpi heap) = findMin heap 

--O(Log n)
add :: Ord a => a -> RAList a -> RAList a
add x (MkR i mpi heap) = MkR (i+1) (assocM i x mpi) (insertH x heap)

--O(n log n)
elems :: Ord a => RAList a -> [a]
elems (MkR i mpi heap) = recolectar 0 i mpi 

--O(n log n) 
--Justificacion:  porque se hace recursion sobre numero igual al tamanio
-- del map, y en cada instancia de la recursion se utiliza
-- lookupM sobre dicho map.
recolectar :: Ord a => Int -> Int -> Map Int a -> [a]
recolectar n t mpi = if n == t
                     then []
                     else case lookupM mpi n of
                     (Just x) -> x : (recolectar (n+1) t mpi)
                     Nothing  -> error "No se cumple la precondicion"

--O(n log n)
remove :: Ord a => RAList a -> RAList a
remove (MkR i mpi heap) =  case lookupM mpi i of 
                           (Just x) ->    MkR (i-1) (deleteM i mpi) (deleteH i heap) 


deleteH :: Ord a => a -> Heap a -> Heap a 
deleteH x heap = if x == findMin 
                 then deleteMin heap  
                 else insertH(findMin) (deleteH x (deleteMin heap))