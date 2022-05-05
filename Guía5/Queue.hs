module Queue (Queue, emptyQ, isEmptyQ, queueQ, firstQ, deQueue)

where 

data Queue a = ConsQ [a]

queue1 = ConsQ [1,2,3]


longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 

emptyQ :: Queue a 
emptyQ = ConsQ []

isEmptyQ :: Queue a -> Bool 
isEmptyQ (ConsQ xs) = null xs

queueQ :: a -> Queue a -> Queue a
queueQ x (ConsQ xs) = ConsQ (agregarAlFinal x xs)

agregarAlFinal :: a -> [a] -> [a] 
agregarAlFinal e []      = e : []
agregarAlFinal e (x:xs)  = x : agregarAlFinal e xs


firstQ :: Queue a -> a
firstQ (ConsQ xs) = head xs

deQueue :: Queue a -> Queue a
deQueue (ConsQ xs) = ConsQ (xs)


instance Show a => Show (Queue a) where 
    show (ConsQ xs) = "{" ++ mostrar xs ++ "}"


mostrar []    = ""
mostrar [x]   = show x 
mostrar(x:xs) =  show x ++ "," ++ mostrar xs