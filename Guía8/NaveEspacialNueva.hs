module Nave(Nave) where 

import Map 
import MaxHeap 
import Tripulante
import Sector 

type Nombre   = String 
type SectorID = String 

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

{-
    Inv. Representación: Con la Nave siendo = N ms mt mht 
                        *Todos los tripulantes que estan en mt DEBEN estar en mht, y viceversa.
                        *En ms el sector asociado al sectorId sId debe tener el sectorId sId.
                        *En mt el tripulante asociado al nombre n debe tener el nombre n.
                        *Dado un tripulante de mt, ese trip. todos los sectores que conoce existen en ms y ese trip. figura en c/u de ellos.
-}      

construir :: [SectorId] -> Nave
--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
--Eficiencia: O(S Log S)
construir sectores = N (asignarSectoresAMap sectores) emptyM emptyH


asignarSectoresAMap :: [SectorID] -> Map SectorID Sector 
--Propósito: Crea sectores vacíos con los sectoresIds en la lista dada.
--Eficiencia O(S Log S)
asignarSectoresAMap []         = emptyM
asignarSectoresAMap (sId:sIds) = let sector = crearSector sId 
                                 in assocM sId sector (asignarSectoresAMap sIds)
                                 
--O(S Log S) por cada sectorId se crea un sector.
--Justificación: *Costo de assocM O(Log S) 
--               *Costo de crearSector O(1).

ingresarT :: Nombre -> Rango -> Nave -> Nave
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
--Eficiencia: O(Log T)
ingresarT  nombre rango (N ms mt mht) = let tripulante = crearTripulante nombre rango in 
                                        N ms (assocM nombre tripulante mt) (insertH tripulante mht)

--Justificación: *Costo de assocM O(Log T) 
--               *Costo de crearTripulante O(1) 
--               *Costo de insertH O(Log T)

sectoresAsignados :: Nombre -> Nave -> Set SectorId
--Propósito: Devuelve los sectores asignados a un tripulante.
--Precondición: Existe un tripulante con dicho nombre.
--Eficiencia: O(log M)
sectoresAsignados nombre (N ms mt mht) = case lookUpM nombre mt of 
                                         Nothing -> error "No se cumple la precondicion" 
                                         Just t  -> sectoresT t

--Justificación: *Costo de lookUpM   O(Log T) 
--               *Costo de sectoresT O(1)  