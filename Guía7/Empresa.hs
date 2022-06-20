import Map 
import Set 
import Empleado 

type SectorId = Int

type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

{-
Inv. Rep. : 
* Los ID de los sectores y los CUIL no se pueden repetir.
* Los empleados que estan en el sectorId tienen que estar en el map del CUIL del empleado.
* Los empleados dentro de si tienen los sectores en los que está ese empleado en el map SectorId del set.
-}

--O(1)
consEmpresa :: Empresa 
consEmpresa = ConsE (EmptyM (EmptyS)) EmptyM 

--O(log n)
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL cuil (ConsE (mapSector(setE)) (mapEmpleado)) = lookUpM cuil mapEmpleado 

--O(logS + E)
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector sectorId (ConsE (mapSector(setE)) (mapEmpleado)) = if belongs sectorId setE 
                                                                      then setToList setE 
                                                                      else []

agregarSectorId :: SectorId -> Empleado -> Empleado
agregarSectorId


--O(n) 
todosLosCUIL :: Empresa -> [CUIL] 
todosLosCUIL (ConsE (mapSector(setE)) (mapEmpleado)) = keys mapEmpleado 

--O(n)
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE (mapSector(setE)) (mapEmpleado)) = keys mapSector 


agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado  sectores cuil (ConsE mapSector mapEmpleado) =  
    
    let empleado = empleadoConSectores sectores (consEmpleado cuil)  

    ConsE(agregarEmpleadoALosSectores sectores empleado mapSector) 
    (assocM cuil empleado mapEmpleado)



consEmpleado :: Cuil -> Empleado


empleadoConSectores :: [SectorId] -> Empleado -> Empleado
empleadoConSectores []     empleado  = empleado  
empleadoConSectores (s:ss) empleado  = agregarSector s (empleadoConSectores ss empleado) --Esto devuelve un empleado con todos los sectores ss.



agregarEmpleadoALosSectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado) 
agregarEmpleadoALosSectores []     empleado  mpSectorId = mpSectorId 
agregarEmpleadoALosSectores (s:ss) empleado  mpSectorId = assocM s (addS empleado (fromJust(lookUpM s mpSectorId))) (agregarEmpleadoALosSectores ss empleado mpSectorId)
--El set me va agregar el empleado al sector y además 



--O(Log S)
agregarSector :: SectorId -> Empresa -> Empresa
agregarSector sid (ConsE mpSectorId mpE) = ConsE (agregarSectorAMap sid mpSectorId) mpE

--O(Log S)
agregarSectorAMap :: SectorId -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarSector sectorId mpSectorId = assocM sectorId emptyS mpSectorId 


agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
--Propósito: agrega un sector al empleado con dicho CUIL.
agregarASector sid cuil (ConsE mpSectorId mpE) = let empleado = agregarSector sid (fromJust(lookUpM c mpE))


borrarEmpleado :: CUIL -> Empresa -> Empresa
--Propósito: elimina al empleado que posee dicho CUIL.
borrarEmpleado cuil (ConsE mpSectorId mpE) = ConsE  
                                            mpSectorId 
                                            (borrarEmpleadoDelMapEmpleado cuil mpe) 

borrarEmpleadoDelMapEmpleado :: CUIL -> Map CUIL Empleado -> Map CUIL Empleado
borrarEmpleadoDelMapEmpleado cuil mapEmpleado = case lookUpM cuil mapEmpleado of 
                                                Just e -> deleteM cuil mapEmpleado 
                                                Nothing -> error "El CUIL no existe"
                                                
 