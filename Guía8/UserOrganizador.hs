import Organizador 
import Map 
import Set


programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 organizador =  if programaronJuntas organizador p1 p2 
                                      then intersection (programasDe organizador p1) (programasDe organizador p2)
                                      else emptyS
--O(log p)
esUnGranHacker :: Organizador -> Persona -> Bool                                      
esUnGranHacker organizador persona = programoATodos (todosLosProgramas organizador) (programasDe organizador persona) 

-- O(log c)
programoATodos :: [Checksum] -> Set Checksum -> Bool 
programoATodos [] setChecksum     = True
programoATodos (x:xs) setChecksum = belongs x setChecksum && (programoATodos xs setChecksum)