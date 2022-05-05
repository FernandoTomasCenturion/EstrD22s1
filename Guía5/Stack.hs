module Stack (Stack, emptyS, isEmptyS, push, top, pop, lenS, stack1) 

where 

data Stack a = ConsStack [a]  

{-
Inv. representación: Ninguna.
-}

stack1 :: Stack Int
stack1 = ConsStack [1,2,3]

instance Show a => Show (Stack a) where 
    show (ConsStack xs) = "{" ++ mostrar xs ++ "}"

mostrar []    = ""
mostrar [x]   = show x 
mostrar(x:xs) =  show x ++ "," ++ mostrar xs

--O(1)
emptyS :: Stack a 
emptyS = ConsStack [] 

--O(1)
isEmptyS :: Stack a -> Bool 
isEmptyS (ConsStack xs) = null xs 

--O(1)
push :: a -> Stack a -> Stack a
push x (ConsStack xs) = ConsStack(x:xs)

--O(1)
top :: Stack a -> a
top (ConsStack xs) = head xs

--O(1)
pop :: Stack a -> Stack a
pop (ConsStack []) = emptyS
pop (ConsStack xs) = (ConsStack (tail xs))

--O(n) siendo n la cantidad de elementos de la lista.
lenS :: Stack a -> Int
lenS (ConsStack xs) = longitud xs 

--O(n) siendo n la cantidad de elementos de la lista.
longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 
