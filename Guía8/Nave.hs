{-
crearS :: SectorId -> Sector O(1)
sectorId :: Sector -> SectorId O(1)
componentesS :: Sector -> [Componente] O(1)
tripulantesS :: Sector -> Set Nombre O(1)
agregarC :: Componente -> Sector -> Sector O(1)
agregarT :: Nombre -> Sector -> Sector O(log T)


crearT :: Nombre -> Rango -> Tripulante O(1)
asignarS :: SectorId -> Tripulante -> Tripulante
O(log S)
sectoresT :: Tripulante -> Set SectorId O(1)
nombre :: Tripulante -> String O(1)
rango :: Tripulante -> Rango O(1)

emptyS :: Set a O(1)
addS :: a -> Set a -> Set a O(log N)
belongsS :: a -> Set a -> Bool O(log N)
unionS :: Set a -> Set a -> Set a O(N log N)
setToList :: Set a -> [a] O(N)
sizeS :: Set a -> Int O(1)

emptyH :: MaxHeap a O(1)
isEmptyH :: MaxHeap a -> Bool O(1)
insertH :: a -> MaxHeap a -> MaxHeap a O(log M)
maxH :: MaxHeap a -> a O(1)
deleteMaxH :: MaxHeap a -> MaxHeap a O(log M)

emptyM :: Map k v O(1)
assocM :: k -> v -> Map k v -> Map k v O(log K)
lookupM :: k -> Map k v -> Maybe v O(log K)
deleteM :: k -> Map k v -> Map k v O(log K)
domM :: Map k v -> [k] O(K)
-}

module Nave(Nave, construir, ingresarT, sectoresAsignados, datosDeSector, tripulantesN, 
            agregarASector, agregarComponenesASector, asignarASector) where 

import Map 
import MaxHeap

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)


{-
Inv. Representacion: 
*El primer map tiene un sectorID que esta asignado a un sector de la nave.
*El segundo map relaciona para cada nombre de tripulante el tripulante de la nave.
*El maxHeap debe estar ordenado de acuerdo al rango de los tripulantes.
*Los sectores asignados al sectorID son tambien conocidos por el tripulante.
*Cada tripulante en el map para cada nombre tiene un nombre.
*Todos los tripulantes que estan en el map están en el maxHeap y vicerversa.
-}

construir :: [SectorId] -> Nave
--Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores
construir sectores = N (construirSectoresId sectores)emptyM emptyH 

construirSectoresId :: [SectorId] -> Map SectorId Sector
construirSectoresId []         = emptyM
construirSectoresId (sid:sids) = let sector = crearS sid 
                                 in  
                                 assocM sector (construirSectoresId sids)

ingresarT :: Nombre -> Rango -> Nave -> Nave
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
ingresarT nombre rango (N mpSector mpTrip mhTrip) = let tripulante = crearT nombre rango 
                                                    in N mpSector (assocM nombre trip mpTrip)
                                                    (insertH tripulante mhTrip)


sectoresAsignados :: Nombre -> Nave -> Set SectorId
--Propósito: Devuelve los sectores asignados a un tripulante
sectoresAsignados nombre (N mpSector mpTrip mhTrip) = case lookUpM nombre mpTrip of 
                                                      Nothing -> error "El tripulante no existe"
                                                      Just t  -> sectoresT t

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
datosDeSector sid (N mpSector mpTrip mhTrip) = case lookUpM sid mpSector of
                                              Nothing -> error "No existe el sector"
                                              Just s  -> (tripulantesS s, componentesS s)


tripulantesN :: Nave -> [Tripulante]                                              
tripulantesN (N mpSector mpTrip mhTrip) = listToRange mhTrip 

listToRange :: MaxHeap Tripulante -> [Tripulante]
listToRange mhTrip =  if isEmptyH 
                      then []
                      else maxH mhTrip : (listToRange(deleteMaxH mhTrip))


agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Asigna una lista de componentes a un sector de la nave
agregarASector componentes sectorID (N mpSector mpTrip mhTrip) = N(agregarComponenesASector componentes sectorID mpSector) mpTrip mhTrip 

agregarComponenesASector :: [Componente] -> SectorId -> Map Sector SectorId -> Map Sector SectorId
agregarComponenesASector componentes sectorId mapSector = assocM sid (agregarComponentes componentes(valorJust (lookUpM sectorId mapSector)) mpSector)


agregarComponentes :: [Componente] -> Sector -> Sector 
agregarComponentes [] sector     = sector
agregarComponentes (c:cs) sector =  agregarC c (agregarComponentes cs sector)

asignarASector :: Nombre -> SectorId -> Nave -> Nave
--Propósito: Asigna un sector a un tripulante.
--Precondición: El tripulante y el sector existen.
asignarASector nombre sectorId (N mpSector mpTrip mhTrip) = N(asignarTripsAMpSector nombre sectorId mpSector) 
                                                             (asignarTripsAMpTrip nombre sectorId mpTrip) 
                                                             (asignarTripsAMhTrip nombre sectorId mhTrip)


asignarTripsAMpSector :: Nombre -> SectorId -> Map SectorId Sector -> Map SectorId Sector
asignarTripsAMpSector nombre sectorId mpSector = assocM (agregarT nombre (valorJust(lookUpM sectorId mpSector)) ) mpSector


asignarTripsAMpTrip :: Nombre -> SectorId -> Map Nombre Tripulante -> Map Nombre Tripulante 
asignarTripsAMpTrip nombre sectorId mpTrip = assocM (asignarS sectorId (valorJust (lookUpM nombre mpTrip)) ) mpTrip


asignarTripsAMhTrip :: Nombre -> SectorId -> MaxHeap Tripulante -> MaxHeap Tripulante 
asignarTripsAMhTrip nombre sectorId mhTrip = if nombre == (maxH mhTrip) 
                                             then insertH (asignarS sectorId (maxH mhTrip)) mhTrip
                                             else asignarTripsAMhTrip nombre sectorId (deleteMaxH mhTrip)





valorJust :: Maybe a -> a 
valorJust (Just v) = v 
valorJust  _       = error "No se encontró el elemento buscado"


