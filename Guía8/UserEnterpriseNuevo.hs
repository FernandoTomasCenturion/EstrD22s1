import Nave 

tripulantes :: Nave -> Set Tripulante
--Eficiencia: O(S Log S) 
tripulantes nave = tripulantesDeSectores (sectores nave)


tripulantesDeSectores :: [Sector] -> Nave -> Set Tripulante 
--Eficiencia: O(Log S)
tripulantesDeSectores []     nave = emptyS
tripulantesDeSectores (s:ss) nave = unionS (tripulantesDe s nave) (ss nave)

--Justificacion: *Costo de unionS        O(S Log S)
--               *Costo de tripulantesDe O(Log S)
--               *Costo final            O(S Log S)