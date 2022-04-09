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
pasosHastaTesoro (Nada camino)       = 1 + (pasosHastaTesoro camino)
pasosHastaTesoro (Cofre objs camino) =  if hayTesoroEnCofre objs 
                                        then 0 
                                        else 1 + pasosHastaTesoro camino