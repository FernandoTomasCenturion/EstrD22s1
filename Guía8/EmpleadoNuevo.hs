module Empresa (Empresa) where

import Map
import Empleado 
import Set

type CUIL     = Int 
type SectorId = Int

data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)
{-
    Inv. Representación: 
        Con la Empresa = siendo ConsE ms mc 
            *En mc el empleado asociado al cuil c debe tener el cuil c.
            *Todos los empleados que  aparecen en el set de un sector s en ms 
             deben estaar en mc, y saber que están en ese sector.
            *Si un empleado e del map mc sabe que está en un sector s, 
             entonces en ms e aparece en set asociado a s.

-} 

consEmpresa :: Empresa 
--O(1)
consEmpresa = emptyM emptyM 

buscarPorCUIL :: CUIL -> Empresa -> Empleado
--Eficiencia: O(Log E)
buscarPorCUIL c (ConsE ms mc) = fromJust(lookUpM c mc)
--Justificación: *Costo de lookUpM O(Log E) 
--               *Costo de fromJust O(1)

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--Propósito: indica los empleados que trabajan en un sector dado.
--Eficiencia: O(Log S + E)
empleadosDelSector sId (ConsE ms mc) = empleadosDelSectorEnMap sId ms 

empleadosDelSectorEnMap :: SectorId -> (Map SectorId (Set Empleado)) -> [Empleado] 
--Eficiencia: O(Log S + E)
empleadosDelSector sId map = case lookUpM map sId of 
                            (Just se) ->  set2List (fromJust se) 
                            Nothing   ->  [] 

--Justificación: *Costo de lookUpM  O(Log S) 
--               *Costo de set2List O(E)   
--               *Costo final:      O(Log S + E)           

todosLosCUIL :: Empresa -> [CUIL]
--Propósito: indica todos los CUIL de empleados de la empresa.
--Eficiencia: O(E)
todosLosCUIL (ConsE ms mc) = keys mc 
--Justificación: *Costo de keys O(E)


todosLosSectores :: Empresa -> [SectorId]
--Propósito: indica todos los sectores de la empresa.
--Eficiencia: O(S)
todosLosSectores (ConsE ms mc) = keys mc
--Justificación: *Costo de keys O(S)

--Preguntar si técnicamente se pisa el valor de la clave cuando el sectorId dado es uno que está dentro de ms.  
agregarSector :: SectorId -> Empresa -> Empresa
--Propósito: Agrega un sector a la empresa, inicialmente sin empleados.
--Precondición: El SectorId dado no debe existir en la empresa 
--Eficiencia: O(Log S)
agregarSector sId (ConsE ms mc) = ConsE (agregarSectorSinEmpleados sId ms) mc

agregarSectorSinEmpleados :: SectorId -> (Map SectorId (Set Empleado)) -> (Map SectorId (Set Empleado))
--Precondición: El SectorId dado no debe existir en la empresa 
--Eficiencia: O(Log S)
agregarSectorSinEmpleados sId ms = case lookUpM ms sId of 
                                    Just s  -> error "No se cumple la precondición" 
                                    Nothing -> assocM sId emptyS map
--Justificación: *Costo de assocM  O(Log S)
--               *Costo de lookUpM O(Log S) 
--               *Costo Final:     O(Log S)

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
--Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá el CUIL dado.
agregarEmpleado sectores c (ConsE ms mc) = ConsE (agregarEmpleadoASectores (consEmpleado c) ms)  
                                                 (assocM c (consEmpleado c) mc)


agregarEmpleadoASectores :: [SectorId] -> Empleado -> (Map SectorId (Set Empleado)) -> (Map SectorId (Set Empleado))
agregarEmpleadoASectores []         e ms  = ms 
agregarEmpleadoASectores (sId:sIds) e ms  = agregarEmpleadoASector sId e (agregarEmpleadoASectores sIds e ms) 
 

agregarEmpleadoASector :: SectorId -> Empleado -> (Map SectorId (Set Empleado)) -> (Map SectorId (Set Empleado))
agregarEmpleadoASector sId e ms = case lookUpM ms sid of 
                                  Nothing  -> error "No se cumple la preco"
                                  (Just s) -> assocM sId (addS e s) ms   

--Preguntar.
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector sId c (ConsE ms mc) = ConsE (agregarEmpleadoASector sId (fromJust(lookUp mc c)) ms) 
                                           (incorporarEmpleadoASector sId c mc)

incorporarEmpleadoASector :: SectorId -> CUIL -> Map CUIL Empleado -> Map CUIL Empleado 
--Precondición: El CUIL dado es válido para un empleado
incorporarEmpleadoASector sId c mc = case lookUpM mc c of 
                                     Nothing -> error "No se cumple la preco"
                                     Just e  -> assocM c (incorporarSector sId e) mc 


borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado c (ConsE ms mc) = let empleado = fromJust (lookUp c mc) 
                                 in ConsE(borrarEmpleadoDeSectores (keys ms) empleado ms)
                                          (remove c mc)

borrarEmpleadoDeSectores :: [SectorId] -> Empleado -> (Map SectorId (Set Empleado)) -> (Map SectorId (Set Empleado)) 
borrarEmpleadoDeSectores []         e ms = ms
borrarEmpleadoDeSectores (sId:sIds) e ms =  let sinEmpleado =  removeS e (fromSet (lookUpM sid ms))
                                            in assocM sId sinEmpleado (borrarEmpleadoDeSectores sids e ms)
  
