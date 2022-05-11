module Queue (Queue, emptyQ, isEmptyQ, firstQ, deQueue)

where 

data Queue a = ConsQ [a] [a] 
                -- La primera lista es fs y la segunda es bs.

{-
  Invariante de representación:
  *Siempre que fs es vacía, bs debe ser vacía y la cola queda vacía.
-}

queue1 = ConsQ [] [2,3,4]

{-
Inv. Representación: Si la primera lista(fs) esta vacía, entonces la cola se encuenta vacía.
-}

--O(1)
emptyQ :: Queue a
emptyQ = ConsQ [] []

--0(1)
isEmptyQ :: Queue a -> Bool 
isEmptyQ (ConsQ fs bs) = null fs 

--O(1)
firstQ :: Queue a -> a 
firstQ (ConsQ fs bs) = head fs 

--O(1)
--Cuando fs es vacía O(n²), siendo n la cantidad de elementos de la cola dada
deQueue :: Queue a -> Queue a 
deQueue (ConsQ fs bs) = colaOrdenada (ConsQ (tail fs) bs) 


--O(1)
-- Cuando fs es vacía O(n²), siendo n la cantidad de elementos de la cola dada
colaOrdenada :: Queue a -> Queue a 
colaOrdenada (ConsQ fs bs) = if null fs 
                             then ConsQ (reversa (bs)) []  
                             else ConsQ fs bs 


