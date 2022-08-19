module Nave (Nave) where 

import Sector 
import Set 
import Tripulante 
import Heap

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
{-
    Inv. Representación: 
      Con la Nave = siendo MkN ms ht (s, n)
             *Si un trip aparece en el set de un sector en ms, entonces ese trip NO aparece en ningún otro.
             *Todos los tripulantes que estan dentro de algun set de tripulantes de ms DEBEN estar en ht, y vicerversa.
             *El sector s existe en ms y la cantidad de tripulantes que tiene en ms  DEBE ser n, y todos los demás sets de ms tienen menos que n elementos.    
-}

naveVacia :: [Sector] -> Nave
--Eficiencia: O(S Log S)
naveVacia sectores = MkN (agregarSectoresSinTripulantes sectores emptyM) emptyH (head (sectores), 0)

agregarSectoresSinTripulantes :: [Sector] -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
agregarSectoresSinTripulantes []     ms = error "La lista de sectores esta vacia" 
agregarSectoresSinTripulantes (s:ss) ms = assocM s emptyS (agregarSectoresSinTripulantes ss ms)
--Justificación: *Costo de assocM:                                  O(Log S)
--               *Por cada cada sector que se agrega hay S sectores O(S)
--Costo final:                                                      O(S Log S)

tripulantesDe :: Sector -> Nave -> Set Tripulante
--Eficiencia: O(Log S)
tripulantesDe s (MkN ms ht (s, n)) = fromJust (lookUpM s ms)
--Justificación: *Costo de assocM: O(Log S)

sectores :: Nave -> [Sector]
--Eficiencia: O(S)
sectores (MkN ms ht (s, n)) = domM ms

conMayorRango :: Nave -> Tripulante
--Eficiencia: O(1)
conMayorRango (MkN ms ht (s, n)) = findMin ht

conMasTripulantes :: Nave -> Sector 
--Eficiencia: O(1)
conMasTripulantes (MkN ms ht (s, n)) = fst (s,n) 

conRango :: Rango -> Nave -> Set Tripulante
--Eficiencia O(Log P)
conRango  rango (MkN ms ht (s,n)) = tripulantesConRango rango ht emptyS 

tripulantesConRango :: Rango -> Heap Tripulante -> Set Tripulante -> Set Tripulante
--Eficiencia: O(Log P)
tripulantesConRango rango ht st = if (rango == rango(findMin ht)) 
                                   then addS (findMin ht) (tripulantesConRango rango (deleteMin ht) st)
                                   else tripulantesConRango rango (deleteMin ht) st
--Justificación: *findMin   O(1) 
--               *deleteMin O(Log P)             
--               *addS      O(Log P)

sectorDe :: Tripulante -> Nave -> Sector
--Propósito: Devuelve el sector en el que se encuentra un tripulante.
--Precondición: el tripulante pertenece a la nave.
--Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
sectorDe trip (MkN ms ht (s,n)) = findMin (sectorDeTripulante trip (domM ms) ms ht)

sectorDeTripulante :: Tripulante -> [Sector] -> Map Sector (Set Tripulante) -> Heap Tripulante ->  Heap Tripulante  
--Eficiencia: O (S Log S + Log T)
sectorDeTripulante trip []     ms ht = ht 
sectorDeTripulante trip (s:ss) ms ht = case lookUpM s ms of 
                                       Just s ->  if belongs t s 
                                                  then insertH t ht 
                                                  else sectorDeTripulante trip ss ms ht 
                                       Nothing -> sectorDeTripulante trip ss ms ht
--Justificación: *Costo de lookUpM  O(Log S) 
--               *Costo de belongsS O(Log S) 
--               *Costo de insertH  O(Log P) 
--Esto se hace por cada sector que hay en la lista dada por parámetro.
--Costo Final: O(S Log S + Log P)

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
--Eficiencia: O(Log P + Log S)
agregarTripulante trip sector (MkN ms ht (s,n)) = MkN (agregarTripulanteEnMap trip sector ms) 
                                                      (agregarTripulanteEnHeap trip ht)
                                                      (verificarMaximoSector sector (s,n) ms) 
--Justificacion: 
--               *Costo de agregarTripulanteEnMap   O(Log P + Log S)
--               *Costo de agregarTripulanteEnHeap  O(Log P)
--               *Costo de verificarMaximoSector    O(Log S)
--               *Costo final por suma de terminos: O(Log P + Log S)


agregarTripulanteEnMap :: Tripulante -> Sector -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante) 
--Efiencia: O(Log P + Log S)
agregarTripulanteEnMap trip sector ms = assocM sector (agregarTripulanteEnSet tripulante (fromJust(lookUpM sector ms))) ms

--Justificacion: *Costo de assocM                 O(Log S)
--               *Costo de lookUpM                O(Log S)
--               *Costo de agregarTripulanteEnSet O(Log P)
--               *Costo final                     O(Log P + Log S)

agregarTripulanteEnSet :: Tripulante -> Set Tripulante -> Set Tripulante 
--Efiencia: O(Log P)
agregarTripulanteEnSet trip st = if (not (belongsS trip st))
                                 then addS trip (agregarTripulanteEnSet trip st)
                                 else agregarTripulanteEnSet trip st

--Justificacion: *Costo de belongsS O(Log P)
--               *Costo de addS     O(Log P)
--               *Costo final       O(Log P)

agregarTripulanteEnHeap :: Tripulante -> Heap Tripulante -> Heap Tripulante 
--Eficiencia O(Log P)
agregarTripulanteEnHeap  trip ht = if trip == findMin ht 
                                   then (insertH trip (deleteMin ht)) 
                                   else insertH trip (agregarTripulanteEnHeap trip ht) 

--Justificación: *Costo de findMin   O(1)
--               *Costo de insertH   O(Log P) 
--               *Costo de deleteMin O(Log P)
--               *Costo final:       O(Log P)

verificarMaximoSector :: Sector -> (Sector, Int) -> Map Sector (Set Tripulante) -> (Sector, Int)
--Eficiencia O(Log S)
verificarMaximoSector sectorAAgregar (s, n) mt = if (cantidadDeTripulantes sectorAAgregar mt) > scn(s, n)
                                                  then (sectorAAgregar, cantidadDeTripulantes sectorAAgregar mt) 
                                                  else (s,n)

-- Justificación: *Costo de cantidadDeTripulantes: O(Log S)


cantidadDeTripulantes :: Sector -> Map Sector (Set Tripulante) -> Int
--Eficiencia: O(Log S)
cantidadDeTripulantes sector ms = sizeS (fromJust (lookUpM sector ms))

--Justificación: *Costo de sizeS   O(1) 
--               *Costo de lookUpM O(Log S)  
--               *Costo final:     O(Log S)

