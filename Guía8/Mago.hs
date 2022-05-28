module Mago(Hechizo, Nombre, Mago, crearM, nombre, aprender, hechizos) where

import Set

type Hechizo = String
type Nombre  = String

data Mago = M Nombre (Set Hechizo) deriving Show

crearM     :: Nombre -> Mago            -- O(1)
nombre     :: Mago -> Nombre            -- O(1)
aprender   :: Hechizo -> Mago -> Mago   -- O(log H)
hechizos   :: Mago -> Set Hechizo       -- O(1)
--(==), (<=) :: Mago -> Mago -> Bool      -- O(1)
-- Iguales, mismo nombre; menor, más hechizos

crearM   n          = M n emptyS
nombre     (M n _)  = n
aprender h (M n sh) = M n (addS h sh)
hechizos   (M _ sh) = sh

instance Eq Mago where
  (M n1 _) == (M n2 _) = n1 == n2

instance Ord Mago where
  (M _ sh1) <= (M _ sh2) = sizeS sh1 >= sizeS sh2




