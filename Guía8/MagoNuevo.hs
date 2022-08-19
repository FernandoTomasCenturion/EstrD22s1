module EscuelaDeMagia(EscuelaDeMagia, fundarEscuela) where 

import Map
import Set 
import PriorityQueue
import Mago

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago) deriving Show

{-
Inv. Rep: Con la escuela = siendo EDM s m p 
*Todos los hechizos de todos los magos en p deben estar en el set s.
*Todos los magos que estan en m deben estar en p, y viceversa.
*En m el mago asociado al nombre n debe tener el nombre n. 
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
                                      Just m  -> EDM setH mapM pqM

-- Eficiencia                         O(Log M)
-- Justificacion: Costo de: *lookUpM  O(Log M) 
--                          *crearM   O(1)
--                          *assocM   O(Log M) 
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

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
--Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
--Precondición: Hay al menos un mago.
--Eficiencia: O(log M)
egregarUno (EDM setH mapM pqM) = let m = findMaxPq pqM in
                                 (m, EDM setH (deleteM (nombre m) mapM) (deleteMaxPQ pqM))
--Justificación: 
--              *Costo de deleteM     O(Log M) 
--              *Costo de deleteMaxPQ O(Log M)
--              *Por suma de logaritmos queda O(Log M)

enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
--Nota: No importa si el mago ya conoce el hechizo dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(M log M + log H)
enseñar hechizo nombre (EDM setH mapM pqM) = case lookUpM nombre mapM of 
                                             Nothing -> error "No se cumple la precondición"
                                             Just m  -> let newM = aprender hechizo m in EDM(addS hechizo setH) (assocM nombre mapM) (modificarPQ newMago pqMago) 
--Costos: *lookUpM     O(Log M)
--        *addS        O(Log H) 
--        *aprender    O(Log H) 
--        *assocM      O(Log M)
--        *modificarPQ O(M Log M)
--Justificación: Los O(Log M) son absorbidos por O(M Log M) y O(Log H) es independiente.


modificarPQ :: Mago -> PriorityQueue Mago -> PriorityQueue Mago 
modificarPQ mago pqM = let maxPQ = findMaxPQ pqM in
                       if (mago == maxPQ) 
                       then insertPQ mago (deleteMaxPQ pqMago)
                       else insertPQ mago (modificarPQ mago (deleteMaxPQ pqMago))
--Eficiencia O(M Log M) 
--Justificacion: *maxPQ        O(1)
--               *findMaxPQ    O(M) 
--               *insertPQ     O(Log M)
--               *deleteMaxPQ  O(Log M)
--Costo final: O(M Log M)

                                        
fromJust :: Maybe v -> v 
fromJust (Just v) = v 