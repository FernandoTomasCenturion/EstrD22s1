module Nave (Nave) where 

import Set 
import Heap
import Map 


data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int) deriving Show
{-
   *Cada tripulante del set esta asignado a un sector como máximo del map.
   *Los tripulantes estan ordenados de mayor a menor rango en la heap.
   *El par sector int guarda al sector con más tripulantes en la nave.
-}

naveVacia :: [Sector] -> Nave
--Propósito: Crea una nave con todos esos sectores sin tripulantes.
--Precondición: la lista de sectores no está vacía
--Costo: O(S log S) siendo S la cantidad de sectores de la lista-
naveVacia sectores = MkN (construirSectoresSinTripulantes sectores) emptyH (head sectores, 0) -- 
--O(s log s) siendo s la cantidad de sectores de la nave.

construirSectoresSinTripulantes :: [Sector] -> Map Sector(Set Tripulante) -> Map Sector(Set Tripulante)
construirSectoresSinTripulantes []     mapSector = mapSector
construirSectoresSinTripulantes (s:ss) mapSector = assocM s emptyS (construirSectoresSinTripulantes ss mapSector)
--O(s log s) siendo s la cantidad de sectores de la nave.
--Por cada sector de la nave se usa assocM, que tiene costo log n.

tripulantesDe :: Sector -> Nave -> Set Tripulante
--Proposito: Denota el conjunto de los tripulantes del sector dado en la nave dada.
tripulantesDe sector (MkN mapSector hpSector maxSector) = case lookUpM sector mapSector of 
                                                         Just setTrips -> setTrips 
                                                         Nothing -> emptyS  
--Costo: O(log S) siendo S la cantidad de sectores

sectores :: Nave -> [Sector]
sectores (MkN mapSector hpSector maxSector) = domM mapSector
--O(S) siendo s la cantidad de sectores de la nave dada. 

conMayorRango :: Nave -> Tripulante 
conMayorRango (MkN mapSector hpSector maxSector) = findMin hpSector 
--O(1) 

conMasTripulantes :: Nave -> Sector 
conMasTripulantes (MkN mapSector hpSector maxSector) = fst(maxSector)
--O(1)

conRango :: Rango -> Nave -> Set Tripulante 
--Propósito: Denota el conjunto de tripulantes con dicho rango.
conRango rango (MkN mapSector hpSector maxSector) = tripulantesConRango rango hpSector

tripulantesConRango :: Rango -> Heap Tripulante -> Sector Tripulante 
tripulantesConRango rango heap sectorTrip = if isEmptyH heap 
                                            then emptyS 
                                            else if rango == rango(findMin heap) 
                                            then addS (findMin heap) (tripulantesConRango rango (deleteMin heap) sectorTrip)
                                            else  tripulantesConRango rango (deleteMin heap) sectorTrip

sectorDe :: Tripulante -> Nave -> Sector
--Propósito: Devuelve el sector en el que se encuentra un tripulante.
--Precondición: el tripulante pertenece a la nave.
sectorDe trip (MkN mapSector hpTrip maxSector) = sectorDelTripulante trip mapSector domM(mapSector)

sectorDelTripulante :: Tripulante  -> Map Sector(Set Tripulante) -> [Sector] -> Sector 
sectorDelTripulante tripulante mapSector (s:ss) = case lookUpM s mapSector of 
                                                  Just tripulantes -> if belongsS tripulante tripulantes 
                                                                      then s 
                                                                      else (sectorDelTripulante tripulante mapSector ss) --Sigo buscando hasta encontrarlo. 
                                                   Nothing -> error "No se cumple la precondicion"                     

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
--Propósito: Agrega un tripulante a ese sector de la nave.
--Precondición: El sector está en la nave y el tripulante no.
agregarTripulante trip sector (MkN mapSector hpTrip maxSector) = MkN (agregarTripulanteConSectorAlMap trip sector mapSector)
                                                                     (agregarTripulanteAlHeap trip hpTrip)
                                                                     (actualizarSector trip sector maxSector)

agregarTripulanteConSectorAlMap :: Tripulante -> Sector -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante) 
agregarTripulanteConSectorAlMap trip sector mapSector = case lookUpM sector mapSector of 
                                                        Just v -> assocM (addS trip v) mapSector 
                                                        Nothing -> error "No se cumple la precondicion"

agregarTripulanteAlHeap :: Tripulante -> Heap Tripulante -> Heap Tripulante 
agregarTripulanteAlHeap trip hpTrip = insertH trip hpTrip  

actualizarSector :: Tripulante -> Sector -> (Sector, Int) -> (Sector, Int) 
actualizarSector trip sector par = if sector == fst(par) 
                                   then (sector, snd(par) + 1)
                                   else par