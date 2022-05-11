module Queue(Queue, emptyQ, isEmptyQ, queueQ, firstQ, deQueue) 

where

data Queue a = ConsQ [a] 

{-
    Inv. Representación: El primer elemento en agregarse a la queue entra por adelante y el elemento que se quita sale por atrás.
-}


queue1 = ConsQ [1,2,3]


longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 

--O(1)
emptyQ :: Queue a 
emptyQ = ConsQ []

--O(1)
isEmptyQ :: Queue a -> Bool 
isEmptyQ (ConsQ xs) = null xs

--O(n)
queueQ :: a -> Queue a -> Queue a
queueQ x (ConsQ xs) = ConsQ(x:xs)

--O(1)
firstQ :: Queue a -> a
firstQ (ConsQ xs) = last xs

--O(1)
deQueue :: Queue a -> Queue a
--Precondición: La lista de la pila dada no está vacía. 
deQueue (ConsQ xs) = ConsQ (init xs) -- init(xs) quita el ultimo elemento de la lista.