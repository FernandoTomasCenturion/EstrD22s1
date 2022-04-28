import Queue 

lengthQ :: Queue a -> Int
lengthQ  x = if (isEmptyQ x) 
                then 0 
                else 1 + (lengthQ(deQueue x))

queueToList :: Queue a -> [a]
queueToList x =  if isEmptyQ x 
                 then []
                 else [firstQ x] ++ (queueToList (deQueue x))
                 
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if emptyQ q2 
               then q1 
               else unionQ (queue (firstQ q2) q1) (deQueue q2)
