module Nave (Nave) where 

import Sector 
import Set 
import Tripulante 

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
{-
    Inv. Representación: 
      Con la Nave = siendo MkN ms ht (s, n)
             *Si un trip aparece en el set de un sector en ms, entonces ese trip NO aparece en ningún otro.
             *Todos los tripulantes que estan dentro de algun de tripulantes de ms DEBEN estar en ht, y vicerversa.
             *El sector s existe en ms y la cantidad de tripulantes que tiene en ms  DEBE ser n, y todos los demás sets de ms tienen menos que n elementos.    
-}