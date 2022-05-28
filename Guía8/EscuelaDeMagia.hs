module EscuelaDeMagia (EscuelaDeMagia, fundarEscuela, estaVacia, magos, registrar, hechizosDe, leFaltanAprender, egresarUno, enseñar)  

where 

import Set 
import Map 
import PriorityQueue
import Mago

data EscuelaDeMagia = EDM (Set Hechizo)  --Todos los enseñados
                          (Map Nombre Mago) --Magos por nombre
                          (PriorityQueue Mago) -- Magos por poder 
                           deriving Show 

{-
  Inv. Representacion: 
  * cada mago en el map para un nombre tiene ese nombre.
  * En la pq no hay 2 magos con el mismo nombre.
  * Todos los hechizos del mago estan en el set. 
  * Todos los magos que estan en el map están en la pq y vicerversa.
-}

--O(1)
fundarEscuela :: EscuelaDeMagia 
fundarEscuela = EDM (emptyS) (emptyM) (emptyPQ)

--O(1)
estaVacia :: EscuelaDeMagia -> Bool 
estaVacia (EDM _ _ pqMago) = isEmptyPQ pqMago

--O(M) donde M es la cantidad de magos en la lista.
magos :: EscuelaDeMagia -> [Nombre] 
magos (EDM _ mapMago _) = keys mapMago

-- O(log M) donde M es la cantidad de magos en la edm.
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar nombre (EDM setH mapMago pqMago) = case (lookUpM nombre mapMago) of 
                                            Nothing -> let m = crearM nombre in EDM setH (assocM nombre m mapMago) (insertPQ m pqMago)
                                            Just m -> EDM setH mapMago pqMago

hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe nombre (EDM setH mapMago pqMago) = case (lookUpM nombre mapMago) of 
                                            Nothing -> error "El mago no existe"
                                            Just m  -> hechizos m

leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender nombre (EDM setH mapMago pqMago) = case (lookUpM nombre mapMago) of 
                                                    Nothing -> error "El mago no exite"
                                                    Just m  -> sizeS setH - sizeS (hechizos m) 

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM setH mapMago pqMago) = let m = findMaxPQ pqMago 
                                       in (m, EDM setH (deleteM (nombre m) mapMago) (deleteMaxPQ pqMago))                          


enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
enseñar hechizo nombre (EDM setH mapMago pqMago) = case lookUpM nombre mapMago of 
                                                   Nothing -> error "El mago no es alumno de la escuela"
                                                   Just m  -> let newMago = aprender hechizo m in EDM (addS hechizo setH) (assocM nombre newMago mapMago) (modificarPQ newMago pqMago)


modificarPQ :: Mago -> PriorityQueue Mago -> PriorityQueue Mago 
modificarPQ mago pqMago = let maxM = findMaxPQ pqMago
                          in if mago == maxM 
                          then insertPQ mago (deleteMaxPQ pqMago)  
                          else insertPQ mago (modificarPQ mago (deleteMaxPQ pqMago))
