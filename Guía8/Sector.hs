module Sector(Sector, SectorId) where 

import Set 
import Tripulante

type SectorId = String

data Sector = Sector SectorId Componente (Set Tripulante) deriving Show
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show 
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show