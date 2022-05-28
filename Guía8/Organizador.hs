
module Organizador (Organizador, Persona, Checksum, nuevo, agregarPrograma, todosLosProgramas, autoresDe, programasDe, programaronJuntas, 
            nroProgramasDePersona, orga) 

where

import Map
import Set

type Persona  = String 
type Checksum = String

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum)) deriving Show
{-
Inv. Rep: 
    -Los programas dentro del checksum tienen los programas en los que está esa persona en el map Checksum del set Persona y viceversa.
-}


--Eficienca O(1)
nuevo :: Organizador 
nuevo = MkO (emptyM) (emptyM)


orga = 
    MkO
        (assocM "React" (addS "Pablov" (addS "Colo" emptyS))(assocM "Angular" (addS "Pablov" emptyS) emptyM))
        (assocM "Colo" (addS "React" emptyS)(assocM "Pablov" (addS "React" (addS "Angular" emptyS)) emptyM))


--
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO porProg porPer) check set = 
    MkO (assocM check set porProg) (agregarAPersona (setToList set) check porPer)

agregarAPersona :: [Checksum] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
agregarAPersona [] check map = map
agregarAPersona (x:xs) check map = 
    assocM x (progDePer x check map) (agregarAPersona xs check map)

progDePer :: Persona -> Checksum -> Map Persona (Set Checksum) -> Set (Checksum)
progDePer p check map =
    case lookUpM p map of
        (Just v) -> addS check v
        Nothing  -> addS check emptyS



--O(C) donde C es la cantidad de códigos identificadores de la lista dada
todosLosProgramas :: Organizador -> [Checksum] 
todosLosProgramas (MkO mpChecksum mpPersona) = keys mpChecksum

autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO mpChecksum mpPersona) checksum =  valor (lookUpM checksum mpChecksum)


valor :: Maybe a -> a 
valor (Just x) = x 

programasDe :: Organizador -> Persona -> Set Checksum 
programasDe (MkO mpChecksum mpPersona) persona = valor (lookUpM persona mpPersona)
 

programaronJuntas :: Organizador -> Persona -> Persona -> Bool 
programaronJuntas organizador p1 p2 = not (isEmptyS(intersection (programasDe organizador p1) (programasDe organizador p2)))

nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona  organizador persona = sizeS (programasDe organizador persona)