import Queue 

--O(n), siendo n la cantidad de elementos de la lista de la cola dada.
--o(n*m) con queue con 2 listas.
lengthQ :: Queue a -> Int
lengthQ  x = if (isEmptyQ x) 
                then 0 
                else 1 + (lengthQ(deQueue x))

--O(n) en queue común y O(n*m) en queue con 2 listas.
queueToList :: Queue a -> [a]
queueToList x =  if isEmptyQ x 
                 then []
                 else [firstQ x] ++ (queueToList (deQueue x))
--O(n*m) en queue común y tambien para queue con 2 listas.        
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if emptyQ q2 
               then q1 
               else unionQ (queue (firstQ q2) q1) (deQueue q2)
