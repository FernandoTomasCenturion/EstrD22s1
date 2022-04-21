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
                              (not(tieneJamon pizza)) && (not (tieneAceitunas pizza))

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

tieneJamon :: Pizza -> Bool 
tieneJamon Prepizza         = False 
tieneJamon (Capa ing pizza) = esJamon ing || tieneJamon pizza


tieneSalsa :: Pizza -> Bool 
tieneSalsa Prepizza         = False 
tieneSalsa (Capa ing pizza) = esSalsa ing || tieneSalsa pizza


tieneAceitunas :: Pizza -> Bool 
tieneAceitunas Prepizza         = False 
tieneAceitunas (Capa ing pizza) = tieneAceitunas pizza || esAceituna ing


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