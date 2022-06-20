import OrganizadorNuevo
import Set 
import Map


programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 orga = intersection (programasDe orga p1) (programasDe orga p2)
--Costo (C log C).

esUnGranHacker :: Organizador -> Persona -> Bool
esUnGranHacker orga persona = programoATodos (todosLosProgramas orga) (programasDe orga persona)
                                --[C]                            --Set C
--Eficiencia: O(P * log C) --Preguntar                               

programoATodos :: [Checksum] -> Set Checksum -> Bool 
programoATodos [] setC      = True 
programoATodos (c:cs) setC  = (belongs c setC) && (programoATodos cs setC)
