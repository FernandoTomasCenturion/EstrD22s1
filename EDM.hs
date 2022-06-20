module EscuelaDeMagia(EscuelaDeMagia) where

import Set 
import Map 
import Mago 
import PriorityQueue

type Hechizo = String 
type Nombre = String

data EscuelaDeMagia = EDM (Set Hechizo) 
                          (Map Nombre Mago) 
                          (PriorityQueue Mago)


{-
    funciones de la interfaz de mago: 
crearM :: Nombre -> Mago O(1)
nombre :: Mago -> Nombre O(1)
aprender :: Hechizo -> Mago -> Mago O(log H)
hechizos :: Mago -> Set Hechizo O(1)


-}


{-
Inv. Rep: *El set de hechizos contiene los hechizos enseñados por la escuela de magia.
          *Cada mago tiene un nombre unico en el map de mago. 
          *Los magos que están en la pq están en el map de mago y se ordenan de mayor a menor, segun la cantidad de hechizos que saben.
-}

--O(1)
fundarEscuela :: EDM 
fundarEscuela = emptyS emptyM emptyPQ 

--O(1)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM setH mapMago pqMago) = isEmptyPQ pqMago 

--O(Log m)
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
-- Propósito: Incorpora un mago a la escuela (si ya existe no hace nada)
registrar nombre (EDM setH mapMago pqMago) = case lookUpM nombre mapMago of 
                                             Just M -> EDM setH mapMago pqMago
                                             Nothing -> let m = crearM nombre in EDM setH (assocM nombre m mapMago) (insertPq m pq )

--O(Log n) donde n es la cantidad de lo nombres de la lista resultante.
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM setH mapMago pqMago) = keys mapMago

--O(Log M) 
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo 
--Precondición: El mago con dicho nombre existe
hechizos nombre (EDM setH mapMago pqMago) = case (lookUpM nombre mapMago) of  
                                            Just m  -> hechizos m 
                                            Nothing -> error "No se cumple la preco"

-- O(log M)
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
--Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
--Precondición: Existe un mago con dicho nombre.
leFaltanAprender nombre (EDM setH mapMago pqMago) = case (lookUpM nombre mapMago) of 
                                                    Nothing -> "El Mago no existe"
                                                    Just m -> sizeS setH -  sizeS (hechizos m) 