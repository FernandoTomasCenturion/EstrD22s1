module Stack(Stack ) where 

data Stack a = ConsStack [a] Int

{-  Inv. Rep:
El int es el numero de la lonigtud de la lista que esta en el stack.
-}


--O(1)
emptyS :: Stack a
emptyS = ConsStack [] 0

--O(1)
isEmptyS :: Stack a -> Bool
isEmptyS (ConsStack xs _) = null xs

--O(1)
push :: a -> Stack a -> Stack a
push x (ConsStack xs n) = ConsStack (x:xs) (n+1)

--O(1)
top :: Stack a -> a
--Precondición: Hay, al menos, un elemento en la pila.
top (ConsStack xs _) = head xs

--O(1)
pop :: Stack a -> Stack a
--Precondición: Hay, al menos, un elemento en la pila.
pop (ConsStack xs n) = ConsStack (tail xs) (n-1)

--O(1)
lenS :: Stack a -> Int
lenS (ConsStack xs n) = n