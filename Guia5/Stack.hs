module Stack (Stack, ) 

where 

data Stack a = ConsStack [a]  

stack1= ConsStack [1,2,3]

instance Show a => Show (Stack a) where 
    show (ConsStack xs) = "{" ++ mostrar xs ++ "}"

mostrar []    = ""
mostrar [x]   = show x 
mostrar(x:xs) =  show x ++ "," ++ mostrar xs

emptyS :: Stack a 
emptyS = ConsStack [] 

isEmptyS :: Stack a -> Bool 
isEmptyS (ConsStack xs) = null xs 

push :: a -> Stack a -> Stack a
push x (ConsStack xs) = (ConsStack(x:xs))

top :: Stack a -> a
top (ConsStack xs) = head xs

pop :: Stack a -> Stack a
pop (ConsStack []) = emptyS
pop (ConsStack xs) = (ConsStack (tail xs))

lenS :: Stack a -> Int
lenS (ConsStack xs) = longitud xs 


longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 
