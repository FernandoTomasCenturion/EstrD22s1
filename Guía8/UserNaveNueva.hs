import Nave 

sectores :: Nave -> Set SectorId
sectores nave  = sectoresAsignadosAT (tripulantesN nave)
--Eficiencia: O(S Log S + Log T)

sectoresAsignadosAT :: [Tripulante] -> Set SectorId 
--Eficiencia (S Log S)
sectoresAsignadosAT []      = emptyS
sectoresAsignadosAT (t:ts)  = union (sectoresT t) (sectoresAsignadosAT ts) 
--Analisis de costos: *unionS    O(S Log S) 
--                    *sectoresT O(1)           
--                               O(S) por cada sector que hay

sinSectoresAsignados :: Nave ->[Tripulante]
sinSectoresAsignados = sinSectoresAsignadosAT (tripulantesN nave)
--Eficiencia O(T) 

sinSectoresAsignadosAT :: [Tripulante] -> [Tripulante]
sinSectoresAsignadosAT []     = []
sinSectoresAsignadosAT (t:ts) = if (size (sectoresT t) == 0)
                                then t: (sinSectoresAsignadosAT ts)
                                else sinSectoresAsignadosAT ts 
--Justificacion:
--                 *size O(1)
--                 *sectoresT O(1) 
--                 Se hace un chequeo por cada de T de la lista dada.
--                 *Costo O(T)                

barriles :: Nave -> [Barril]
barriles nave = barrilesDe (sectores nave)
--Efiencia O(C + S Log S)

barrilesDe :: Set SectorID -> Nave -> [Barril] 
barrilesDe ssId nave = barrilesDeComponente (asignacionesPorSector (setToList ssId) nave)
--Eficiencia O(C + S Log S + S)
--Justificación: *setToList O(S) 
--               *asignacionesPorSector  O(C + S Log S)


barrilesDeComponente :: [(Set Nombre, [Componente])] -> [Barril]
barrilesDeComponente []           = []
barrilesDeComponente ((sn, c):cs) = barrilesC c ++ barrilesDeComponente cs
--Eficiencia O(C) donde c es la cantidad de componentes.

asignacionesPorSector :: [SectorId] -> Nave -> [(Set Nombre, [Componente])] 
--O(S* Log S)
asignacionesPorSector []           nave = []
asignacionesPorSector (sId: sIds)  nave = (datosDeSector sId nave) : (asignacionesPorSector sIds nave)
--Justificación: *datosDeSector          O(Log S)
--               *asignacionesPorSector  O(S * Log S) porque hay S sectoresId en la lista dada 