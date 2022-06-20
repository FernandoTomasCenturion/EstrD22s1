module EscuelaDeMagia(EscuelaDeMagia, fundarEscuela) where 

import Map
import Set 
import PriorityQueue
import Mago

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago) deriving Show

{-
Inv. Rep: Con la escuela = EDM s m p 
Todos los hechizos de todos los magos en p deben estar en el set s.
Todos los magos que estan en m deben estar en p, y viceversa.
En m el mago asociado al nombre n debe tener el nombre n. 
-}

fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM emptyS emptyM emptyPQ 
--O(1)

estaVacia :: EscuelaDeMagia -> Bool 
estaVacia (EDM setH mapM pqM) = isEmptyPQ pqM
--O(1)

registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Preco: El mago con el nombre dado no existe en la EDM.
registrar nombre (EDM setH mapM pqM) = case lookUpM nombre mapM of
                                      Nothing -> let m = crearM nombre in EDM setH (assocM nombre m mapM) (insertPQ m pqM) 
                                      Just m -> EDM setH mapM pqM
--Eficiencia O(Log M)
-- Justificacion: Costo de: *lookUpM O(Log M) 
--                          *crearM O(1)
--                          *assocM O(Log M) 
--                          *insertPQ O(Log M)      

magos :: EscuelaDeMagia -> [Nombre]
magos (EDM setH mapM pqM) = keys mapM 
--Eficiencia: O(M) en peor caso, donde M es la cantidad de magos de la escuela dada.


hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
--Preco: El mago con el nombre dado existe en la edm.
hechizosDe nombre (EDM setH mapM pqM) = case lookUpM nombre mapM of 
                                        Just m -> hechizos m  --Preguntar el tema de fromJust.
                                        Nothing -> error "El mago no existe"
--Eficiencia: O(Log M) en peor caso, donde M es la cantidad de magos en de la escuela dada.
--Justificación: Costo de: *lookUpM O(Log M)
--                         *fromJust O(1).


leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
--Existe un mago con dicho nombre
leFaltanAprender n (EDM setH mapM pqM) = case lookUpM n mapM of 
                                         Nothing -> error "No se cumple la precondicion"
                                         Just m ->  sizeS setH - sizeS (hechizos m)
--Eficiencia: O(Log M) en peor caso, donde M es la cantidad de magos de la escuela dada.


fromJust :: Maybe v -> v 
fromJust (Just v) = v 