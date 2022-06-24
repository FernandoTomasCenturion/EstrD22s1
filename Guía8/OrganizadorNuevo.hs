module OrganizadorNuevo (Organizador, Persona, Checksum, nuevo, agregarPrograma, todosLosProgramas, autoresDe, programasDe, programaronJuntas, 
            nroProgramasDePersona, orga) 

where

import Map
import Set
import Persona 
import Checksum


data OrganizadorNuevo = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
{-
    Inv. Representacion: 
* Para cada elemento p del conjunto de pers. del Checksum c en el primer map, 
  c pertenece a conjunto de Checksums del p en el segundo map.  
* Para cada elemento c del conjuto de checksums de la persona p en el segundo map, 
  p aparece en el conjunto del checksum c en el primer map.        
-}

nuevo :: Organizador
--Eficiencia O(1)
nuevo = emptyM emptyM

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
--Propósito:    Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
--              de dicho programa.
--Precondición: El identificador del programa que se agrega no fue usado previamente en el organizador, 
--              y el Set de personas no está vacío.
--Eficiencia:   O (P * (Log P+ Log C)) 
agregarPrograma (MkO mapChecksum mpPersona) checksum setP = MkO (agregarChecksumAMap mapChecksum checksum setP)
                                                                (agregarAPersonaChecksums (setToList setP) checksum mpPersona)

--Justificación: * Costo de agregarChecksumAMap          O(Log C) 
--               * Costo de agregarAPersonaChecksums     O(P * (Log P+ Log C)) 
--               * Por suma de términos semejantes queda O(P * (Log P+ Log C)) 

agregarChecksumAMap :: (Map Checksum(Set Persona)) -> Checksum -> Set Persona -> (Map Checksum(Set Persona))
--Precondición: El checksum no esta en el map dado.
--Eficiencia:   O(Log C)
agregarChecksumAMap map checksum set = case lookUpM map checksum of 
                                      Just s  -> error "No se cumple la precondicion"
                                      Nothing -> assocM checksum set map
--Justificación: Costo de lookUpM O(Log C). Costo de assocM O(Log C). Por suma de logaritmos queda O(Log C)

agregarAPersonaChecksums :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> (Map Persona (Set Checksum))
--Eficiencia:   O (P * (Log P+ Log C))
agregarAPersonaChecksums []     c  map  = map 
agregarAPersonaChecksums (p:ps) c  map  = case lookUpM map p of 
                                        Just  s -> assocM p (addS c s) (agregarAPersonaChecksums ps c map)
                                        Nothing -> assocM p (addS c emptyS) (agregarAPersonaChecksums ps c map) 
--Justificación: Costo de lookUpM O(Log P) donde P es la cantidad de Personas en el Map.
              -- Costo de assocM es O(Log P).
              -- Costo de addS O(Log C). Estos 3 suceden por cada persona.

todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO mapC mapP) = domM mapC 
--Eficiencia: O (Log C) donde C es la cantidad de códigos identificadores del map.

autoresDe :: Organizador -> Checksum -> Set Persona
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.   
autores (MkO mapC mapP) checksum = case lookUpM mapC checksum of 
                                   Just s -> (fromJust s) 
                                   Nothing -> error "No se cumple la precondicion"                                
--Justificación: Costo de lookUpM es O(Log C) y el resto de las operaciones son constantes.

programasDe :: Organizador -> Persona -> Set Checksum
--Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe (MkO mapC mapP) p =  fromJust (lookUpM mapP p)
--Justificación: Costo de lookUpM es O(Log P) y el resto de las operaciones son constantes.

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
--Precondición: Las personas dadas DEBEN ser distintas.
programaronJuntas orga p1 p2 = not (isEmptyS(insertection (programasDe orga p1) (programasDe orga p2))) 
-- Eficiencia:   O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
--               programas del organizador, y C la cantidad total de programas.
--Justificación: Costo de programasDe  es  O (log P).
--               Costo de Interseccion es  O (C log C). Estos 2 suceden por la cantidad de programas.

nroProgramasDePersona :: Organizador -> Persona -> Int
--Eficiencia:    O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
nroProgramasDePersona orga p = sizeS(programasDe orga p)
--Justificacion: Costo de programasDe O(log P).
