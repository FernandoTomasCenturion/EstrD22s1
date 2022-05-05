module Queue (Queue, emptyQ, isEmptyQ, firstQ)

where 

data Queue a = ConsQ [a] [a]
                -- La primera lista es fs y la segunda es bs.

{-
  Invariante de representación:
  *Siempre que fs es vacía, bs debe ser vacía y la cola queda vacía.
-}


queue1 = ConsQ [1,2,3] []

--O(n*m) sendo in es la cantidad de elementos de la lista y m es el costo de agregar al final.
reversa :: [a] -> [a] 
reversa []     = []
reversa (x:xs) =  agregarAlFinal (reversa xs) x

--O(m) ya que m es la cantidad de elementos de la lista.
agregarAlFinal :: [a] -> a -> [a] 
agregarAlFinal []     a  = [a]
agregarAlFinal (x:xs) a  = x : (agregarAlFinal xs a)

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
deQueue :: Queue a -> Queue a 
deQueue (ConsQ fs bs) = colaOrdenada (ConsQ (tail fs) bs) 


--O(1)
--Cuando fs es vacía, el orden es n*m, siendo n la cantidad de elementos de la lista y siendo m el costo heredado de reversa.
colaOrdenada :: Queue a -> Queue a 
colaOrdenada (ConsQ fs bs) = if null fs 
                             then (reversa (bs)) []  
                             else Q fs bs 


