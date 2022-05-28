module BST(BST)

where 

data BST a = EmptyBST | NodeBST a (BST a) (BST a) deriving Show
{-
Inv. Representación: 
Siendo NodeBST x ti td un arbol binario 
Entonces: 
1-Los elementos de ti son menores a x 
2-Los elementos de td son mayores a x
3-Los arboles ti y td también cumplen las condiciones de 1 y 2.
4- No hay elementos repetidos 
-}

--Subtarea implementada para BST.
heightT :: BST a -> Int
heightT EmptyBST                  = 0
heightT (NodeBST x t1 t2)         = 1 + (max (heightT t1) (heightT t2))



emptyBST :: BST a 
emptyBST  = EmptyBST 

--Costo O(log n) siendo n la cantidad de elementos del árbol.
belongsBST :: Ord a => a -> BST a -> Bool 
belongsBST e EmptyBST           = False 
belongsBST e (NodeBST x ti td)  = if e == x
                                  then True 
                                  else if e < x 
                                      then belongsBST e ti 
                                      else belongsBST e td    
--O(log n)
insertBST :: Ord a => a -> BST a -> BST a 
insertBST e EmptyBST          = NodeBST e EmptyBST EmptyBST
insertBST e (NodeBST x ti td) = if e == x 
                                then NodeBST x ti td 
                                else if e < x 
                                    then NodeBST x (insertBST e ti) td 
                                    else NodeBST x ti (insertBST e td) 
--O(log n) siendo n la cantidad de elementos del arbol 
deleteBST :: Ord a => a -> BST a -> BST a
deleteBST e EmptyBST = EmptyBST 
deleteBST e (NodeBST x ti td) = if e == x 
                               then rearmarBST ti td 
                               else if e < x 
                                   then NodeBST x (deleteBST e ti) td 
                                   else NodeBST x ti (deleteBST e td )

--O(log n)
rearmarBST :: Ord a => BST a -> BST a -> BST a
rearmarBST ti td = NodeBST (minBST td) ti (deleteMinBST td) 


minBST :: BST a -> a 
minBST EmptyBST = error "no hay minimo"
minBST (NodeBST x EmptyBST td) = x
minBST (NodeBST x ti td) = minBST ti 

deleteMinBST :: BST a -> BST a 
deleteMinBST EmptyBST = EmptyBST
deleteMinBST (NodeBST x EmptyBST td) = td
deleteMinBST (NodeBST x ti td) = NodeBST x (deleteMinBST ti) td 

t1= NodeBST 2 -- raiz 
    (NodeBST 1 EmptyBST EmptyBST) -- rama izquierda
    (NodeBST 3 EmptyBST EmptyBST) -- rama derecha

--O(log t) donde t es la cantidad de elementos del BST.
--El costo logaritmico se cumple siempre y cuando el arbol sea avl.
splitMaxBST :: Ord a => BST a -> (a, BST a) 
splitMaxBST EmptyBST                = error "No hay Maximo"
splitMaxBST (NodeBST x ti EmptyBST) = (x, ti)
splitMaxBST (NodeBST x ti td)       = let (max, t) = splitMaxBST td 
                                      in  (max, NodeBST x ti t) 

splitMinBST :: Ord a => BST a -> (a, BST a) 
splitMinBST EmptyBST                = error "No hay elemento minimo" 
splitMinBST (NodeBST x EmptyBST td) = (x, td)
splitMinBST (NodeBST x ti td)       = let (min, t) = splitMinBST ti 
                                      in (min, NodeBST x t td)
 

esBST :: Ord a => BST a -> Bool 
esBST EmptyBST          = True 
esBST (NodeBST x ti td) = esMenorARaiz x td && esMayorARaiz x ti 
                            && esBST ti && esBST td 


esMayorARaiz :: Ord a => a -> BST a -> Bool 
esMayorARaiz x EmptyBST = True 
esMayorARaiz x t        = x > root t


esMenorARaiz :: Ord a => a -> BST a -> Bool 
esMenorARaiz x EmptyBST = True 
esMenorARaiz x t        = x < root t

root :: BST a -> a 
--Precondición hay una raíz.
root (NodeBST x ti td) = x

elMaximoMenorA :: Ord a => a -> BST a -> Maybe a
elMaximoMenorA x EmptyBST          = Nothing 
elMaximoMenorA x (NodeBST y ti td) = if x > y 
                                     then elegirMaybe y (elMaximoMenorA x td) 
                                     else elMaximoMenorA x ti



elMinimoMayorA :: Ord a => a -> BST a -> Maybe a
elMinimoMayorA x EmptyBST          = Nothing
elMinimoMayorA x (NodeBST y ti td) = if x < y 
                                     then elegirMaybe y (elMinimoMayorA x ti) 
                                     else elMinimoMayorA x td                                     


elegirMaybe :: a -> Maybe a -> Maybe a 
elegirMaybe x Nothing = Just x 
elegirMaybe x y       = y
 


--Para que un arbol esté balanceado la dif. de alturas debe ser menor o igual a 1.
balanceado :: BST a -> Bool
balanceado EmptyBST          = True
balanceado (NodeBST x ti td) = abs (heightT ti - heightT td) <= 1 && balanceado ti && balanceado td
