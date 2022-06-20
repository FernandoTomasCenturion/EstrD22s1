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
nuevo = emptyM emptyM


agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
--Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
--de dicho programa.
--Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, 
--y el Set de personas no está vacío.
--Eficiencia: no hay ninguna garantía de eficiencia
agregarPrograma (MkO mapChecksum mpPersona) checksum setP = MkO (agregarChecksumAMap mapChecksum checksum setP)
                                                                (agregarAPersonaChecksums (setToList setP) checksum mpPersona)

--Costo: O(Log C + P Log P)


agregarChecksumAMap :: (Map Checksum(Set Persona)) -> Checksum -> Set Persona -> (Map Checksum(Set Persona))
--Precondición: El checksum no esta en el map dado.
agregarChecksumAMap map checksum set = case lookUpM map checksum of 
                                      Just s  -> error "No se cumple la precondicion"
                                      Nothing -> assocM checksum set map
--Eficiencia:  O(Log C)
--Justificación: Costo de lookUpM O(Log C), costo de assocM O(Log C).

agregarAPersonaChecksums :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> (Map Persona (Set Checksum))
agregarAPersonaChecksums []     c  map  = map 
agregarAPersonaChecksums (p:ps) c  map  = case lookUpM map p of 
                                        Just  s -> assocM p (addS c s) (agregarAPersonaChecksums ps c map)
                                        Nothing -> assocM p (addS c emptyS) (agregarAPersonaChecksums ps c map)
--Eficiencia: O (P * (Log P+ Log C)) 
--Justificación: Costo de lookUpM O(Log P) donde P es la cantidad de Personas en el Map.
                 --Costo de assocM es O(Log P).
                 --costo de addS O(Log C). Estos 3 suceden por cada persona.
                


todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO mapC mapP) = domM mapC 
--Eficiencia: O (Log C) donde C es la cantidad de códigos identificadores del map.

autoresDe :: Organizador -> Checksum -> Set Persona
autores (MkO mapC mapP) checksum = case lookUpM mapC checksum of 
                                   Just s -> (fromJust s) 
                                   Nothing -> error "No se cumple la precondicion"
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.                                   
--Justificación: Costo de lookUpM es O(Log C) y el resto de las operaciones son constantes.

programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO mapC mapP) p =  fromJust (lookUpM mapP p)
--Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
--Justificación: Costo de lookUpM es O(Log P) y el resto de las operaciones son constantes.

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
--Precondición: Las personas dadas DEBEN ser distintas.
programaronJuntas orga p1 p2 = not (isEmptyS(insertection (programasDe orga p1) (programasDe orga p2))) 
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
--programas del organizador, y C la cantidad total de programas.
--Justificación: Costo de programasDe O(log P).
               --Costo de Interseccion es O (C log C). Estos 2 suceden por la cantidad de programas.

nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona orga p = sizeS(programasDe orga p)
--Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
--Justificacion: Costo de programasDe O(log P).
