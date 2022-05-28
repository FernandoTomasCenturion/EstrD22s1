module Tripulante (Tripulante, Nombre, Rango) where 

import Set
import Sector

type Nombre = String 
type Rango = String 


data Tripulante = Tripulante Nombre Rango (Set SectorId) deriving Show

{-
Inv. Representación: Ninguna.
-}

