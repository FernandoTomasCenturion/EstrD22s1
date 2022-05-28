import Nave  

sectores :: Nave -> Set SectorId
--Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores nave = sectoresConTripulantesAsignados (tripulantesN nave) 

sectoresConTripulantesAsignados :: [Tripulante] -> Set SectorId 
sectoresConTripulantesAsignados []     = emptyS
sectoresConTripulantesAsignados (t:ts) = unionS (sectoresT t) (sectoresConTripulantesAsignados ts)

sinSectoresAsignados :: Nave ->[Tripulante]
sinSectoresAsignados nave = sinSectoresAsignadosTrip (tripulantesN nave) 

sinSectoresAsignadosTrip :: [Tripulante] -> [Tripulante] 
sinSectoresAsignadosTrip []     = []
sinSectoresAsignadosTrip (t:ts) = if (sizeS sectoresT t) == 0 
                                  then t: (sinSectoresAsignadosTrip ts) 
                                  else sinSectoresAsignadosTrip ts 

barriles :: Nave -> [Barril]
barriles nave = barrilesDe (sectores nave) nave 

barrilesDe :: Set Sector -> Nave -> [Barril]
barrilesDe setSector nave = barrilesDeComponente (asignacionesPorSector(setToList setSector) nave) 

barrilesDeComponente :: [(Set Nombre, [Componente])] -> [Barril]
barrilesDeComponente []     = []
barrilesDeComponente (c:cs) = obtenerBarriles c  ++ barrilesDeComponente cs

