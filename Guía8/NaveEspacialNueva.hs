module Nave(Nave) where 

import Map 
import MaxHeap 
import Tripulante
import Sector 

type Nombre   = String 
type SectorID = String 

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]

data Barril = Comida | Oxigeno | Torpedo | Combustible


barrilesDeComponente :: Componente -> [Barril] 
barrilesDeComponente  (Almacen barrilesC) = barrilesC
barrilesDeComponente _                    = []



{-
    Inv. Representación: Con la Nave siendo = N ms mt mht 
                        *Todos los tripulantes que estan en mt DEBEN estar en mht, y viceversa.
                        *En ms el sector asociado al sectorId sId debe tener el sectorId sId.
                        *En mt el tripulante asociado al nombre n debe tener el nombre n.
                        *Dado un tripulante de mt, ese trip. de todos los sectores que conoce que existen en ms y ese trip. figura en c/u de ellos.
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

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
--Eficiencia: O(log S)
datosDeSector sId (N ms mt mht) = case lookUpM sId ms of 
                                 Nothing -> error "No se cumple la precondicion"
                                 Just s  -> (tripulantesS s, componentesS s)

--Justificación: Costos: 
--                         *lookUpM      O(Log S)
--                         *tripulantesS O(1)
--                         *componentesS O(1) 
--              CostoFinal:              O(Log S)

tripulantesN :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
--Eficiencia: O(Log T)
tripulantesN (N ms mt mht) = tripulantesOrdenados mht 



tripulantesOrdenados :: MaxHeap Tripulante -> [Tripulante] 
--Proposito: Devuelve una lista de tripulantes ordenada por rango, de mayor a menor de la mht dada.
--Eficiencia O(Log T)
tripulantesOrdenados  mht = if isEmptyH 
                             then []
                             else maxH mht : (tripulantesOrdenados(deleteMax mht))
--Justificación: Costos: 
--                      *maxH O(1)  
--                      *deleteMax O (Log T)
--Costo Final:          *O(Log T)       


agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Asigna una lista de componentes a un sector de la nave
agregarASector componentes sId (N ms mt mht) = N (agregarASectorEnMap componentes sId ms) mt mht 
-- O(C + Log S)


agregarASectorEnMap :: [Componente] -> SectorId -> (Map SectorId Sector) -> (Map SectorId Sector) 
agregarASector componentes sId ms = assocM sId (agregarASectorEnSet componentes (fromJust(lookUpM sId ms)) ms)
--Justificación: Costos: assocM              O(Log S) 
--                       lookUpM             O(Log S) 
--                       agregarASectorEnSet O(C)
--Costo Final O(C + Log S)

agregarASectorEnSet :: [Componente] -> Sector -> Sector 
agregarASectorEnSet []     s = ... 
agregarASectorEnSet (c:cs) s = agregarC c s (agregarASectorEnSet cs s)
--Justificación: Costos: *agregarC O(1) 
--                       *agregarASectorEnSet O(C) 
--                       *Costo Final: O(C) 


asignarASector :: Nombre -> SectorId -> Nave -> Nave
--Propósito: Asigna un sector a un tripulante.
--Nota: No importa si el tripulante ya tiene asignado dicho sector.
--Precondición: El tripulante y el sector existen.
--Eficiencia: O(log S + log T + T log T)
asignarASector nombre sId (N ms mt mht) = N (asignarASectorEnMap sId ms) 
                                            (asignarTripEnMap nombre sid mt)
                                            (asignarTripEnMaxHeap nombre sid mht)

asignarASectorEnMap :: SectorId -> Map SectorId Sector -> Map SectorId Sector 
--Prop: Asigna un sector a un tripulante en el sectorId dado con el map dado por parámetro.
--Prec: El sectorId existe.
--Eficiencia: O(Log S + Log T)
asignarASectorEnMap sId ms = assocM sId (agregarT nombre (fromJust (lookUpM sid ms))) ms
--Justificacion: *Costo de assocM   O(Log S) 
--               *Costo de lookUpM  O(Log S) 
--               *Costo de fromJust O(1) 
--               *Costo de agregarT O(Log T) 
--Por suma de costos queda: O(Log S + Log T)

asignarTripEnMap :: Nombre -> SectorId -> Map Nombre Tripulante -> Map Nombre Tripulante 
--Prop: Asigna un tripulante con el nombre y sId dado al map dado por parámetro.
--Precondición: El tripulante y el sector existen.
--Eficiencia: O(Log S + Log T)
asignarTripEnMap nombre sId mt = assocM nombre (asignarS sId (fromJust(lookUpM nombre mt)) ) mt
--Justificación: *Costo de assocM   O(Log T)
--               *Costo de asignarS O(Log S)
--               *Costo de fromJust O(1) 
--               *Costo de lookUpM  O(Log T)
--Por suma de costos queda:         O(Log S + Log T)

asignarTripEnMaxHeap ::  Nombre -> SectorId -> MaxHeap Tripulante -> MaxHeap Tripulante 
asignarTripEnMaxHeap nombre sId mht = if (nombre == nombre(maxH mth)) 
                                      then insertH(asignarS sid (maxH mth)) mht 
                                      else asignarTripEnMaxHeap nombre sId (deleteMax mht)
--Justificación: *Costo de nombre    O(1)
--               *Costo de insertH   O(Log T)
--               *Costo de asignarS  O(Log S) 
--               *Costo de maxH      O(1) 
--               *Costo de deleteMax O(Log T)
--Por suma de costos queda: O(Log S + Log T)

