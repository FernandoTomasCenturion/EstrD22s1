data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

celda0:: Celda 
celda0 = CeldaVacia

celda1 :: Celda 
celda1 = Bolita Azul celda0 

celda2 :: Celda 
celda2 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))

nroBolitas :: Color -> Celda -> Int 
nroBolitas c (CeldaVacia)         = 0
nroBolitas c (Bolita color celda) = unoSi(bolitaEsDelMismoColor c color) + nroBolitas c celda

color :: Celda -> Color 
color (Bolita color celda) = color

unoSi:: Bool -> Int 
unoSi True     = 1
unoSi False    = 0

bolitaEsDelMismoColor :: Color -> Color -> Bool 
bolitaEsDelMismoColor  Azul Azul = True 
bolitaEsDelMismoColor  Rojo Rojo = True 
bolitaEsDelMismoColor  _    _    = False

poner :: Color -> Celda -> Celda
poner c  CeldaVacia           = (Bolita c CeldaVacia)
poner c (Bolita color celda)  = (Bolita c(Bolita color celda))

sacar :: Color -> Celda -> Celda 
sacar c CeldaVacia           = CeldaVacia 
sacar c (Bolita color celda) = if bolitaEsDelMismoColor c color 
                               then celda 
                               else (Bolita color (sacar c celda))

ponerN :: Int -> Color -> Celda -> Celda 
ponerN  0 c celda  = celda
ponerN  n c celda  = (Bolita c (ponerN (n-1) c celda))                              

data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show 


camino0 :: Camino 
camino0 = Fin 

camino1 :: Camino 
camino1 = Nada camino0 

camino2 :: Camino 
camino2 = Cofre [Cacharro, Cacharro, Tesoro] camino1 

--- Preguntar sobre las pruebas.
camino3 :: Camino 
camino3 = Cofre [Cacharro, Cacharro] camino2 

hayTesoro :: Camino -> Bool
hayTesoro (Cofre objs camino) = hayTesoroEnCofre objs 
hayTesoro Fin                 = False
hayTesoro (Nada camino)       = False || hayTesoro camino

hayTesoroEnCofre :: [Objeto] -> Bool 
hayTesoroEnCofre []         = False
hayTesoroEnCofre (obj:objs) = esTesoro obj || hayTesoroEnCofre objs

esTesoro :: Objeto -> Bool 
esTesoro  Tesoro = True 
esTesoro  _      = False   

pasosHastaTesoro :: Camino -> Int 
--Precondición: Hay al menos un tesoro
pasosHastaTesoro  Fin           = 0
pasosHastaTesoro (Nada c)       = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre objs c) = if hayTesoro c
                                  then 1+ pasosHastaTesoro c
                                  else pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool 
hayTesoroEn n camino = pasosHastaTesoro camino == n

alMenosNTesoros :: Int -> Camino -> Bool 
alMenosNTesoros n camino = n <= cantidadDeTesoros camino


cantidadDeTesoros :: Camino -> Int 
cantidadDeTesoros Fin                  = 0
cantidadDeTesoros (Nada camino)        = unoSi(hayTesoro camino) + cantidadDeTesoros camino
cantidadDeTesoros (Cofre objs camino)  = cantidadDeTesorosEnCofre objs + cantidadDeTesoros camino

cantidadDeTesorosEnCofre :: [Objeto] -> Int 
cantidadDeTesorosEnCofre  []         = 0
cantidadDeTesorosEnCofre  (obj:objs) = unoSi (esTesoro obj) + cantidadDeTesorosEnCofre objs

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arbolConUnNodo :: Tree Int 
{-  .
-}
arbolConUnNodo = NodeT 1 (EmptyT) (EmptyT)

arbolConUnNodoIzquierdo:: Tree Int
{-
   .  .
  /    \
 .      .
-}
--Preguntar como se representa un arbol.
arbolConUnNodoIzquierdo = NodeT 1 (NodeT 2(NodeT 3 EmptyT EmptyT) (NodeT 5 EmptyT EmptyT)) (NodeT 6 (NodeT 7 EmptyT EmptyT ) EmptyT) 


sumarT :: Tree Int -> Int 
sumarT EmptyT          = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

sizeT :: Tree Int -> Int 
sizeT EmptyT           = 0
sizeT (NodeT n t1 t2)  = 1 + (sizeT t1) + (sizeT t2) 

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = (NodeT (n*2) (mapDobleT t1) (mapDobleT t2))


perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT           = False
perteneceT x (NodeT n t1 t2)  = (x==n) || (perteneceT x t1) || (perteneceT x t2)

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT  x EmptyT          = 0
aparicionesT  x (NodeT n t1 t2) = if x == n  
                                  then 1 + (aparicionesT x t1) + (aparicionesT x t2) 
                                  else (aparicionesT x t1) + (aparicionesT x t2) 
leaves :: Tree a -> [a]
leaves EmptyT                  = []
leaves (NodeT x EmptyT EmptyT) = [x] 
leaves (NodeT x ti td)         = leaves ti ++ leaves td

heightT :: Tree a -> Int
heightT EmptyT                  = 0
heightT (NodeT x EmptyT EmptyT) = 1
heightT (NodeT x t1 t2)         = 1 + (heightT t1) + (heightT t2)