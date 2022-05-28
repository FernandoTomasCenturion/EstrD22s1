import EscuelaDeMagia 
import Map 
import Set 
import PriorityQueue 
import Mago 

hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto edm = not (estaVacia edm) && hayUnMagoYEsExperto edm  

hayUnMagoYEsExperto :: EscuelaDeMagia -> Bool 
hayUnMagoYEsExperto edm = let m = fst (egresarUno edm)
                          in leFaltanAprender (nombre m) edm == 0

egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos edm = if not (hayUnExperto edm) 
                      then ([], edm) 
                      else let  (m, escuelaSinM) = egresarUno edm 
                                (ms, escuelaSinMs) = egresarExpertos escuelaSinM 
                           in   (m:ms, escuelaSinMs)                           