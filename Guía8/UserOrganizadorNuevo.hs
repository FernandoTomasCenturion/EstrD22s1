--Implementación del usuario TAD de Organizador.
import OrganizadorNuevo
import Set 
import Map

--Funciones como usuario de TAD Organizador
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 orga = intersection (programasDe orga p1) (programasDe orga p2)
--Eficiencia:    O(P Log P + Log P)
--Justificación: 
--               *Costo de intersection O(P Log P).
--               *Costo de programasDe  O(Log P).

esUnGranHacker :: Organizador -> Persona -> Bool
--Propósito: Denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
esUnGranHacker orga persona = programoATodos (todosLosProgramas orga) (programasDe orga persona)
                                                   --[C]               --Set C           
--Eficiencia: O(C * Log C + Log P)                           
--Costo de todosLosProgramas O(Log C) 
--Costo de programasDe O(Log P)
--Costo de programoATodos O(C * Log C)

programoATodos :: [Checksum] -> Set Checksum -> Bool 
--Propósito: Denota verdadero si algún checksum de la lista dada pertenece al conjunto de checksum dado.
programoATodos [] setC      = True 
programoATodos (c:cs) setC  = (belongs c setC) && (programoATodos cs setC)
--Costo de belongs O(Log C)
--La lista tiene c checksums en peor caso. Quedaría el costo de O(C) 
--Eficiencia O(C* Log C)
