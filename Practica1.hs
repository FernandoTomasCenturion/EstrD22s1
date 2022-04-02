--Lo que está del lado izquierdo es un parámetro.

sucesor :: Int -> Int 
sucesor n = n + 1

doble :: Int -> Int 
doble n = n * 2

sumar :: Int -> Int -> Int 
sumar n m = n + m

divisionYResto :: Int -> Int -> (Int, Int) 
divisionYResto n m = (div n m, mod n m)

maxDelPar :: (Int,Int) -> Int
maxDelPar (n,m) =   if n > m 
                     then n 
                     else m

data Dir = Norte | Este | Sur | Oeste
       deriving   Show


siguienteDir :: Dir -> Dir 
--Precondición: La ultima dirección es Oeste.
siguienteDir Este   = Sur 
siguienteDir Norte  = Este
siguienteDir Sur    = Oeste
siguienteDir Oeste  = Norte


opuesto :: Dir -> Dir 
opuesto Norte    = Sur
opuesto Sur      = Norte
opuesto Este     = Oeste 
opuesto Oeste    = Este 
-- sumar 2 (sumar 3 4) los parentesís se usan para agrupar, como en matemática.

iguales :: Dir -> Dir -> Bool 
iguales Este  Este  = True 
iguales Norte Norte = True 
iguales Sur   Sur   = True 
iguales Oeste Oeste = True 
iguales _     _     = False

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 
     deriving Show 

primeroYUltimoDia:: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia  = (Lunes, Domingo)  


empiezaConM :: DiaDeSemana -> Bool 
empiezaConM Martes       = True 
empiezaConM Miercoles    = True 
empiezaConM _            = False 

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool 
vieneDespues Lunes Martes     = True 
vieneDespues Martes Miercoles = True 
vieneDespues Miercoles Jueves = True 
vieneDespues Jueves Viernes   = True 
vieneDespues Viernes Sabado   = True 
vieneDespues Sabado  Domingo  = True 
vieneDespues Domingo Lunes    = True 
vieneDespues _ _              = False 


estaEnElMedio :: DiaDeSemana -> Bool 
estaEnElMedio diaDeSemana = numeroDelDiaDeSemana diaDeSemana > 1 && numeroDelDiaDeSemana diaDeSemana < 7 

numeroDelDiaDeSemana :: DiaDeSemana -> Int 
--Dado un día de semana, describe su número.
numeroDelDiaDeSemana Lunes     = 1 
numeroDelDiaDeSemana Martes    = 2
numeroDelDiaDeSemana Miercoles = 3
numeroDelDiaDeSemana Jueves    = 4 
numeroDelDiaDeSemana Viernes   = 5 
numeroDelDiaDeSemana Sabado    = 6
numeroDelDiaDeSemana Domingo   = 7


negar :: Bool -> Bool 
negar True     = False 
negar False    = True


implica :: Bool -> Bool -> Bool
implica True False  = False 
implica _  _        = True    

andLogico :: Bool -> Bool -> Bool 
andLogico True b     = b
andLogico False _    = False 

orLogico :: Bool -> Bool -> Bool 
orLogico True b     = True
orLogico False b    = b 


type Nombre = String 
type Edad   = Int

data Persona = ConsP Nombre Edad   deriving Show 
                     --Nombre --Edad
                                 

tomas :: Persona 
tomas = ConsP "Tomas" 24      

nicolas :: Persona 
nicolas = ConsP "Nicolas" 30
 
nombre :: Persona -> String 
nombre  (ConsP n e) = n       


edad :: Persona -> Int
edad  (ConsP n e) = e       

crecer :: Persona -> Persona 
crecer  consP = ConsP (nombre consP) ((edad consP) + 1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombre (ConsP _ edad) = ConsP nombre edad 

esMayorQueLaOtra :: Persona -> Persona -> Bool 
esMayorQueLaOtra p1 p2 = edad p1 > edad p2 

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2) then p1 
                      else p2 


type NombrePokemon = String
type PorcentajeEnergia = Int

data TipoDePokemon = Agua | Fuego | Planta deriving Show


data Pokemon    = ElPokemon TipoDePokemon PorcentajeEnergia  deriving Show

data Entrenador = ElEntrenador NombrePokemon Pokemon Pokemon  deriving Show

pokemon1 :: Pokemon
pokemon1  = ElPokemon Agua 100 

pokemon2 :: Pokemon
pokemon2  = ElPokemon Planta 50

pokemon3 :: Pokemon
pokemon3  = ElPokemon Fuego 50

pokemon4 :: Pokemon
pokemon4  = ElPokemon Agua 50

ash1 :: Entrenador
ash1 = ElEntrenador "ash1" pokemon1 pokemon2 

ash2 ::Entrenador 
ash2 = ElEntrenador "ash2" pokemon3 pokemon4

superarA  :: Pokemon -> Pokemon -> Bool
superarA (ElPokemon t1 _) (ElPokemon t2 _) = puedeSuperarA t1 t2 


--Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.

puedeSuperarA :: TipoDePokemon -> TipoDePokemon -> Bool 
puedeSuperarA     Agua  Fuego    = True 
puedeSuperarA     Fuego Planta   = True 
puedeSuperarA     Planta Agua    = True 
puedeSuperarA    _    _          = False 

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe  tp (ElEntrenador _ p1 p2) = cantidadDe tp p1 p2


cantidadDe :: TipoDePokemon -> Pokemon -> Pokemon -> Int 
cantidadDe   tp (ElPokemon t1 _) (ElPokemon t2 _) = unoSi (esDelMismoTipo tp t1) + 
                                                    unoSi (esDelMismoTipo tp t2)

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool 
esDelMismoTipo   Agua Agua      = True
esDelMismoTipo   Fuego Fuego    = True
esDelMismoTipo   Planta Planta  = True
esDelMismoTipo   _  _           = False

unoSi:: Bool -> Int 
unoSi True     = 1
unoSi False    = 0 



--Devuelve uno si se cumple la condición, si no devuelve 0


juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (ElEntrenador _ p1 p2, ElEntrenador _ p3 p4) = [p1, p2, p3, p4] 

loMismo :: a -> a 
loMismo x = x 

siempreSiete:: a -> Int
siempreSiete  x = 7

swap :: (a,b) -> (b,a) 
swap (x,y) = (y,x) 


estaVacia :: [a] -> Bool 
estaVacia  []  = True 
estaVacia _    = False 

sinElPrimero :: [a] -> [a]
sinElPrimero  (_:xs) = xs 

--Preguntar los 2 casos que surgieron
splitHead :: [a] -> (a, [a])
splitHead   (x:xs) = (x, xs) 
