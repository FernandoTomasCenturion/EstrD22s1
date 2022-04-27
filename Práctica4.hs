import Distribution.Simple.LocalBuildInfo (componentBuildDir)
data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
 
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show 

pizza0 = Prepizza 

pizza1 = Capa Salsa pizza0 

pizza2 = Capa Queso pizza1

pizza3 = Capa (Aceitunas 8) pizza2

pizza4 = Capa Jamon pizza3

pizza5 = Capa Salsa pizza4

ingredientes1= [Aceitunas 8, Queso, Jamon, Salsa]

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza                 = 0
cantidadDeCapas (Capa ingrediente pizza) = 1 + cantidadDeCapas pizza 


armarPizza :: [Ingrediente] -> Pizza 
armarPizza []         = Prepizza 
armarPizza (ing:ings) = Capa ing (armarPizza ings) 

sacarJamon :: Pizza -> Pizza 
sacarJamon Prepizza         = Prepizza
sacarJamon (Capa ing pizza) = if esJamon ing 
                              then sacarJamon pizza
                              else Capa ing (sacarJamon pizza) 

esJamon :: Ingrediente -> Bool 
esJamon Jamon = True 
esJamon _     = False 

tieneSoloSalsaYQueso :: Pizza -> Bool 
tieneSoloSalsaYQueso  pizza = tieneQueso pizza && tieneSalsa pizza && 
                              (not(tieneJamonYAceitunas pizza))

tieneQueso :: Pizza -> Bool 
tieneQueso Prepizza         = False 
tieneQueso (Capa ing pizza) = esQueso ing || tieneQueso pizza

esQueso :: Ingrediente -> Bool 
esQueso Queso = True
esQueso _     = False 

esSalsa :: Ingrediente -> Bool 
esSalsa Salsa = True 
esSalsa _     = False

esAceituna :: Ingrediente -> Bool 
esAceituna (Aceitunas _) = True 
esAceituna _             = False

tieneJamonYAceitunas :: Pizza -> Bool 
tieneJamonYAceitunas Prepizza         = False 
tieneJamonYAceitunas (Capa ing pizza) = esJamon ing || esAceituna ing || tieneJamonYAceitunas pizza


tieneSalsa :: Pizza -> Bool 
tieneSalsa Prepizza         = False 
tieneSalsa (Capa ing pizza) = esSalsa ing || tieneSalsa pizza



duplicarAceitunas :: Pizza -> Pizza 
duplicarAceitunas Prepizza         = Prepizza 
duplicarAceitunas (Capa ing pizza) = if esAceituna ing 
                                     then Capa (duplicarCapaDeAceituna ing) (duplicarAceitunas pizza) 
                                     else Capa ing (duplicarAceitunas pizza) 

duplicarCapaDeAceituna :: Ingrediente ->Ingrediente 
duplicarCapaDeAceituna (Aceitunas cantidad) = Aceitunas (cantidad * 2)

cantDeCapasPorPizza :: [Pizza] -> [(Int, Pizza)] 
cantDeCapasPorPizza []     = []
cantDeCapasPorPizza (p:ps) = (cantidadDeIngredientesDePizza p, p) : cantDeCapasPorPizza ps

cantidadDeIngredientesDePizza :: Pizza -> Int 
cantidadDeIngredientesDePizza pizza = longitud (sinIngredientesRepetidos (ingredientesUsados pizza))


sinIngredientesRepetidos :: [Ingrediente] -> [Ingrediente]
sinIngredientesRepetidos []     = []
sinIngredientesRepetidos (i:is) = let is' = sinIngredientesRepetidos is
                                     in if perteneceIngrediente i is'
                                         then     is'
                                         else i : is' 


perteneceIngrediente :: Ingrediente -> [Ingrediente] -> Bool
perteneceIngrediente i []         = False
perteneceIngrediente i (ing:ings) = sonElMismoIngrediente i ing || perteneceIngrediente i ings    

ingredientesUsados :: Pizza -> [Ingrediente] 
ingredientesUsados Prepizza = [] 
ingredientesUsados (Capa ing pizza) = ing : ingredientesUsados pizza

sonElMismoIngrediente :: Ingrediente -> Ingrediente -> Bool
sonElMismoIngrediente Queso Queso = True
sonElMismoIngrediente Salsa Salsa = True
sonElMismoIngrediente Jamon Jamon = True
sonElMismoIngrediente _ _ = False

longitud :: [a] -> Int 
longitud  []    = 0
longitud (x:xs) = 1 + longitud xs


data Dir = Izq | Der deriving Show

data Objeto = Tesoro | Chatarra deriving Show

data Cofre = Cofre [Objeto] deriving Show

data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

cofre1 = Cofre [Tesoro, Chatarra, Tesoro] 

cofre2 = Cofre [Chatarra, Chatarra, Tesoro]

cofre3 = Cofre [Chatarra, Tesoro, Chatarra]

cofre5 = Cofre []

mapa1 = Fin cofre1
mapa2 = Fin cofre2

mapa3 = Bifurcacion cofre3 mapa1 mapa2



hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre)               = hayTesoroEnCofre cofre 
hayTesoro (Bifurcacion cofre mi md) = hayTesoroEnCofre cofre || hayTesoro mi || hayTesoro md


hayTesoroEnCofre :: Cofre -> Bool 
hayTesoroEnCofre (Cofre objs) = hayTesoroEnObjetos objs 

hayTesoroEnObjetos :: [Objeto] -> Bool 
hayTesoroEnObjetos []         = False
hayTesoroEnObjetos (obj:objs) = esTesoro obj || hayTesoroEnObjetos objs

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True 
esTesoro _      = False

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] mapa = hayTesoroEnEstePunto mapa 
hayTesoroEn (d:ds) (Fin cofre) = error "No hay mas camino"
hayTesoroEn (d:ds) (Bifurcacion cofre mi md) =
    case d of 
        Izq -> hayTesoroEn ds mi 
        Der -> hayTesoroEn ds md

hayTesoroEnEstePunto :: Mapa -> Bool
hayTesoroEnEstePunto (Fin cofre)             = hayTesoroEnCofre cofre 
hayTesoroEnEstePunto (Bifurcacion cofre _ _) = hayTesoroEnCofre cofre


caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre)               = []
caminoAlTesoro (Bifurcacion cofre mi md) = if hayTesoroEnCofre cofre 
                                            then [] 
                                            else elegirCaminoAlTesoro (Bifurcacion cofre mi md)



elegirCaminoAlTesoro :: Mapa -> [Dir]
elegirCaminoAlTesoro (Bifurcacion cofre mi md) = if hayTesoro mi 
                                                 then Izq : caminoAlTesoro mi 
                                                 else Der : caminoAlTesoro md 


caminoDeLaRamaMasLarga :: Mapa -> [Dir] 
caminoDeLaRamaMasLarga (Fin cofre)               = []
caminoDeLaRamaMasLarga (Bifurcacion cofre mi md) =  if heightM mi > heightM md 
                                                    then Izq : caminoDeLaRamaMasLarga mi 
                                                    else Der : caminoDeLaRamaMasLarga md      



heightM :: Mapa -> Int
heightM (Fin cofre)                    = 0
heightM (Bifurcacion cofre mi md)      = 1 + max (heightM mi) (heightM md)

tesorosPorNivel :: Mapa -> [[Objeto]] 
tesorosPorNivel (Fin cofre)               = [tesorosEn cofre] 
tesorosPorNivel (Bifurcacion cofre mi md) = tesorosEn cofre : juntarNiveles (tesorosPorNivel mi) (tesorosPorNivel md)


tesorosEn :: Cofre -> [Objeto] 
tesorosEn (Cofre objs) = filtrarTesorosEnObjetos objs


filtrarTesorosEnObjetos :: [Objeto] -> [Objeto]
filtrarTesorosEnObjetos []          = []
filtrarTesorosEnObjetos (obj:objs)  = singularSi (esTesoro obj) obj ++ filtrarTesorosEnObjetos objs

singularSi :: Bool -> a -> [a]
singularSi True  x = [x]
singularSi _ _     = []


juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []   yss          = yss ---Primero lista vacía y después el caso donde no está vacía.
juntarNiveles xss     []        = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys)  :  juntarNiveles xss yss

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin cofre)               = []
todosLosCaminos (Bifurcacion cofre mi md) = (prepend Izq (todosLosCaminos mi)) ++ (prepend Der (todosLosCaminos md))


prepend :: Dir -> [[Dir]] -> [[Dir]]
prepend d []       = []
prepend d (dir:dirs) = (d:dir) : (prepend d dirs)


data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)

sectores :: Nave -> [SectorId]
sectores (N treeSector) = sectoresDe treeSector 

sectoresDe :: Tree Sector -> [SectorId]
sectoresDe EmptyT               = []
sectoresDe (NodeT sector si sd) = idDeSector sector : (sectoresDe si ++ sectoresDe sd)

idDeSector :: Sector -> SectorId 
idDeSector (S sectorId _ _ ) = sectorId

poderDePropulsion :: Nave -> Int
poderDePropulsion (N treeSector) = propulsionAcumuladaEntreSectores treeSector

propulsionAcumuladaEntreSectores :: Tree Sector -> Int 
propulsionAcumuladaEntreSectores EmptyT               = 0
propulsionAcumuladaEntreSectores (NodeT sector si sd) = poderDeMotorDeSector sector + propulsionAcumuladaEntreSectores si + propulsionAcumuladaEntreSectores sd 

poderDeMotorDeSector ::  Sector -> Int 
poderDeMotorDeSector (S _ componentes _ ) = poderDeMotorEntreComponentes componentes


poderDeMotorEntreComponentes :: [Componente] -> Int 
poderDeMotorEntreComponentes []      = 0
poderDeMotorEntreComponentes (c:cs)  = if esMotor c 
                                       then poderDeMotor c + poderDeMotorEntreComponentes cs 
                                       else poderDeMotorEntreComponentes cs

esMotor :: Componente -> Bool 
esMotor (Motor _ ) = True 
esMotor _          = False                                

poderDeMotor :: Componente -> Int 
poderDeMotor (Motor potencia) = potencia 


barriles :: Nave -> [Barril] 
barriles (N treeSector) = listaDeBarriles treeSector 

listaDeBarriles :: Tree Sector -> [Barril]
listaDeBarriles EmptyT               = []
listaDeBarriles (NodeT sector si sd) = barrilesDeSector sector ++ listaDeBarriles si ++ listaDeBarriles sd

barrilesDeSector :: Sector -> [Barril] 
barrilesDeSector (S _ componente _ ) =  barrilesDeLosComponentes componente

barrilesDeLosComponentes :: [Componente] -> [Barril] 
barrilesDeLosComponentes []     = []
barrilesDeLosComponentes (c:cs) = if esAlmacen c 
                                  then barrilesDeAlmacen c ++ barrilesDeLosComponentes cs 
                                  else barrilesDeLosComponentes cs 

esAlmacen :: Componente -> Bool 
esAlmacen (Almacen xs ) = True 
esAlmacen _            = False 

barrilesDeAlmacen :: Componente -> [Barril] 
barrilesDeAlmacen (Almacen barriles ) = barriles 

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sId (N treeSector) = N (buscarSectorYAgregarComponentes cs sId treeSector) 

buscarSectorYAgregarComponentes :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
buscarSectorYAgregarComponentes cs sid EmptyT               = EmptyT
buscarSectorYAgregarComponentes cs sid (NodeT sector si sd) = if sid == idDeSector sector 
                                                              then NodeT (agregarEnSectorActual sector cs) si sd 
                                                              else  NodeT sector (buscarSectorYAgregarComponentes cs sid si) (buscarSectorYAgregarComponentes cs sid sd)

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA nTrip listIdS (N treeSector) = N (sectoresLuegoDeAsignacion nTrip listIdS treeSector)

sectoresLuegoDeAsignacion :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector 
sectoresLuegoDeAsignacion nTrip listIdS EmptyT = EmptyT
sectoresLuegoDeAsignacion nTrip [] treeSector = treeSector
sectoresLuegoDeAsignacion nTrip listIdS (NodeT sector si sd) = 
    if perteneceSector (idDeSector sector) listIdS
        then NodeT (asignarAlTripulanteEnSector nTrip sector) (sectoresLuegoDeAsignacion nTrip listIdS si) (sectoresLuegoDeAsignacion nTrip listIdS sd)
        else NodeT sector (sectoresLuegoDeAsignacion nTrip listIdS si) (sectoresLuegoDeAsignacion nTrip listIdS sd)  


perteneceSector :: SectorId -> [SectorId] -> Bool 
perteneceSector sectorId []         = False
perteneceSector sectorId (sid:sids) = sectorId == sid || perteneceSector sid sids
 
asignarAlTripulanteEnSector :: Tripulante -> Sector -> Sector 
asignarAlTripulanteEnSector nTrip (S id comps tripulacion) = S id comps (nTrip : tripulacion)

agregarEnSectorActual :: Sector -> [Componente] -> Sector
agregarEnSectorActual (S sid componentes trip) compNuevos =(S sid (componentes ++ compNuevos) trip) 


sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tripBuscado (N treeSector) = losSectoresDelTripulante tripBuscado treeSector

losSectoresDelTripulante :: Tripulante -> Tree Sector -> [SectorId]
losSectoresDelTripulante tripBuscado EmptyT = []
losSectoresDelTripulante tripBuscado (NodeT sector si sd) = 
    if tieneAsignacionEnSector tripBuscado sector 
        then (idDeSector sector) : (losSectoresDelTripulante tripBuscado si ++ losSectoresDelTripulante tripBuscado sd)
        else losSectoresDelTripulante tripBuscado si ++ losSectoresDelTripulante tripBuscado sd

tieneAsignacionEnSector :: Tripulante -> Sector -> Bool 
tieneAsignacionEnSector tripBuscado (S _ _ tripulantes) = elTripulantePertenece tripBuscado tripulantes


tripulantes :: Nave -> [Tripulante]
tripulantes (N treeSector) = tripulantesDe treeSector 

tripulantesDe :: Tree Sector -> [Tripulante] 
tripulantesDe EmptyT               = []
tripulantesDe (NodeT sector si sd) = sinTripulantesRepetidos (tripulantesDelSector sector ++ tripulantesDe si ++ tripulantesDe sd) 

tripulantesDelSector :: Sector -> [Tripulante]
tripulantesDelSector (S _ _ trip)  = trip 


sinTripulantesRepetidos :: [Tripulante] -> [Tripulante]
sinTripulantesRepetidos []     = []
sinTripulantesRepetidos (t:ts) = let ts' = sinTripulantesRepetidos ts
                                     in if elTripulantePertenece t ts'
                                        then     ts'
                                        else t : ts'  
                            
elTripulantePertenece ::  Tripulante -> [Tripulante] -> Bool 
elTripulantePertenece   trip []     = False    
elTripulantePertenece   trip (t:ts) = (trip == t) || elTripulantePertenece trip ts 


type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo


lobo1 :: Lobo
lobo1 = Cazador "Tom" ["p1","p2","p3","p4","p5"] lobo2 lobo3 lobo4

lobo2 :: Lobo
lobo2 = Explorador "Nico" ["t1","t2"] lobo5 lobo6

lobo3 :: Lobo
lobo3 = Cazador "Julio" ["p1","p2"] lobo7 lobo8 lobo9

lobo4 :: Lobo
lobo4 = Cria "Cria1"

lobo5 :: Lobo
lobo5 = Cria "Cria2"

lobo6 :: Lobo
lobo6 = Cria "Cria3"

lobo7 :: Lobo
lobo7 = Cria "Cria4"

lobo8 :: Lobo
lobo8 = Cria "Cria5"

lobo9 :: Lobo
lobo9 = Cazador "Pitchiot" ["p1","p2","p2","p2","p2","p2","p2","p2","p2","p2"] lobo4 lobo5 lobo6



manada :: Manada
manada = M lobo1

buenaCaza :: Manada -> Bool
buenaCaza (M lobo) = cantidadDePresas lobo > cantidadDeCrias lobo

cantidadDePresas :: Lobo -> Int
cantidadDePresas (Cria _)                         = 0
cantidadDePresas (Explorador _ _ lob1 lob2)       = (cantidadDePresas lob1)+(cantidadDePresas lob2)
cantidadDePresas (Cazador _ presa lob1 lob2 lob3) = cantPresa presa +(cantidadDePresas lob1)+(cantidadDePresas lob2)+(cantidadDePresas lob3)


cantPresa :: [Presa] -> Int
cantPresa ps = length ps

cantidadDeCrias :: Lobo ->Int
cantidadDeCrias (Cria _)                     = 1
cantidadDeCrias (Explorador _ _ lob1 lob2)   = (cantidadDeCrias lob1 ) + (cantidadDeCrias lob2)
cantidadDeCrias (Cazador _ _ lob1 lob2 lob3) = cantidadDeCrias lob1 + cantidadDeCrias lob2 + cantidadDeCrias lob3

elAlfa :: Manada -> (Nombre, Int) 
elAlfa  (M lobo) = elAlfaLobo lobo 


elAlfaLobo :: Lobo -> (Nombre, Int) 
elAlfaLobo (Cria nombre) = (nombre, 0)
elAlfaLobo (Cazador nombre presa lobo1 lobo2 lobo3) =
    maximo
    (maximo (nombre, length presa) (elAlfaLobo lobo1))
    (maximo (elAlfaLobo lobo2 ) (elAlfaLobo lobo3))

elAlfaLobo (Explorador nombre territorios lobo1 lobo2) =
    maximo
    (maximo (nombre, 0) (elAlfaLobo lobo1))
    (elAlfaLobo lobo2) 

maximo :: (Nombre, Int ) -> (Nombre , Int )  -> (Nombre , Int ) 
maximo  (name1, nro1) (name2, nro2) = if  nro1 > nro2 then (name1, nro1) else (name2, nro2)


losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronL t l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cria n) = []
losQueExploraronL t (Explorador n ts l1 l2) = if perteneceTerritorio t ts
		                                      then n : losQueExploraronL t l1 ++ losQueExploraronL t l2 
                                               else losQueExploraronL t l1 ++  losQueExploraronL    t l2
losQueExploraronL t (Cazador n ps l1 l2 l3) = losQueExploraronL t l1 ++  losQueExploraronL t l2 ++  losQueExploraronL t l3


exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio manada = exploradoresPorTerritorioManada manada (territoriosSinRep manada)  

exploradoresPorTerritorioManada :: Manada -> [Territorio] -> [(Territorio, [Nombre])]
exploradoresPorTerritorioManada manada []  = [] 
exploradoresPorTerritorioManada manada (t:ts) =  generadorDeTupla t (losQueExploraron t manada) : exploradoresPorTerritorioManada manada ts 


generadorDeTupla :: a -> b -> (a, b)
generadorDeTupla a b = (a, b)


territoriosSinRep :: Manada -> [Territorio]
territoriosSinRep (M l) = territoriosSinRepLobo l

territoriosSinRepLobo :: Lobo -> [Territorio]
territoriosSinRepLobo (Cria n) = []
territoriosSinRepLobo (Cazador n ps l1 l2 l3) = sinTerritoriosRepetidos (territoriosSinRepLobo l1 ++ territoriosSinRepLobo l2 ++ territoriosSinRepLobo l3)
territoriosSinRepLobo (Explorador n ts l1 l2) = sinTerritoriosRepetidos (ts ++ territoriosSinRepLobo l1 ++ territoriosSinRepLobo l2)

perteneceTerritorio :: Territorio -> [Territorio] -> Bool 
perteneceTerritorio territorio []     = False
perteneceTerritorio territorio (t:ts) = territorio == t || perteneceTerritorio territorio ts

sinTerritoriosRepetidos :: [Territorio] -> [Territorio]
sinTerritoriosRepetidos []     = []
sinTerritoriosRepetidos (t:ts) = let ts' = sinTerritoriosRepetidos ts
                                     in if perteneceTerritorio t ts'
                                        then     ts'
                                        else t : ts'                             

superioresDelCazador :: Nombre -> Manada -> [Nombre]
--Precondición : hay un cazador con dicho nombre y es único
superioresDelCazador nombre (M lobo) = superioresDelCazadorLobo nombre lobo

superioresDelCazadorLobo :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorLobo nombre (Cria n1)                = []
superioresDelCazadorLobo nombre (Explorador n1 ts l1 l2) = superioresDelCazadorLobo nombre l1 ++ superioresDelCazadorLobo nombre l2
superioresDelCazadorLobo nombre (Cazador n1 ps l1 l2 l3) =  if nombre == n1
                                                    then []
                                                    else n1 : superioresDelCazadorLobo nombre l1 ++ superioresDelCazadorLobo nombre l2 ++ superioresDelCazadorLobo nombre l3                                        