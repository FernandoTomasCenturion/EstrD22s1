data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

pizza0 = Prepizza

pizza1= Capa Salsa pizza0

pizza2= Capa Jamon pizza1

pizza3= Capa (Aceitunas 8) pizza2

cantidadDeCapas :: Pizza -> Int 
cantidadDeCapas Prepizza    = 0
cantidadDeCapas (Capa i p)  = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
armarPizza []          = Prepizza
armarPizza (ing:ings)  = Capa ing (armarPizza ings) 


sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza     = Prepizza
sacarJamon (Capa ing p) = if esJamon ing
                          then sacarJamon p 
                          else Capa ing (sacarJamon p)


duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) = Capa (duplicarCapaDeAceitunas ing) (duplicarAceitunas p)


duplicarCapaDeAceitunas :: Ingrediente -> Ingrediente 
duplicarCapaDeAceitunas (Aceitunas n) = Aceitunas (n * 2)
duplicarCapaDeAceitunas ingrediente   = ingrediente 

ingredientes :: Pizza -> [Ingrediente] 
ingredientes Prepizza     = []
ingredientes (Capa ing p) = ing : ingredientes p

esJamon :: Ingrediente -> Bool 
esJamon Jamon = True 
esJamon  _    = False

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []           = [] 
cantCapasPorPizza (p:ps)  = (cantidadDeCapas p, p) : cantCapasPorPizza ps
