--Recursión sobre listas.

sumatoria :: [Int] -> Int 
sumatoria []      = 0
sumatoria (x:xs)  = x + sumatoria xs  

longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 


--- para sea el sucesor de cada nro, le tengo que sumar 1 a cada elemento de la lista
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) =  (x + 1) : sucesores xs 


conjuncion :: [Bool] -> Bool
conjuncion []      = True
conjuncion (x:xs)  = x && conjuncion xs 



disyuncion :: [Bool] -> Bool
disyuncion []      = error "No hay elementos en la lista"
disyuncion (b:[])  = b
disyuncion (x:xs)  = x || disyuncion xs 

aplanar :: [[a]] -> [a] 
aplanar []       = []
aplanar (xs:xss) = concatenar xs (aplanar xss)

pertenece :: Eq a => a -> [a] -> Bool 
pertenece   a []     = False    
pertenece   a (x:xs) = (a == x) || pertenece a xs 

---Por cada aparicion de "a" sumo 1.
apariciones :: Eq a => a -> [a] -> Int
apariciones a []     = 0
apariciones a (x:xs) = unoSi(a == x) + apariciones a xs
                        
unoSi:: Bool -> Int 
unoSi True     = 1
unoSi False    = 0 

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA  n []       = []
losMenoresA  n (x:xs)   =  if (esMayorA n x)
                           then x : losMenoresA n xs    
                           else losMenoresA n xs


esMenorA:: Int -> Int -> Bool 
esMenorA x y = x < y 


esMayorA :: Int -> Int -> Bool 
esMayorA n z=  n > z


agregarAlFinal :: [a] -> a -> [a] 
agregarAlFinal []     a  = [a]
agregarAlFinal (x:xs) a  = x : (agregarAlFinal xs a)
 
concatenar :: [a] -> [a] -> [a] 
concatenar [] ys     = ys 
concatenar (x:xs) ys = x : concatenar xs ys


lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []         = []
lasDeLongitudMayorA n (x:xs)     = if    (longitud x > n)   
                                   then  x : lasDeLongitudMayorA n xs
                                   else  lasDeLongitudMayorA n xs

reversa :: [a] -> [a] 
reversa []     = []
reversa (x:xs) =  agregarAlFinal (reversa xs) x


elMinimo :: Ord a => [a] -> a 
--Precondicion: La lista no debe ser vacía
elMinimo (x:[])     = x 
elMinimo (x:xs)     = minimoEntre x (elMinimo xs) 


minimoEntre :: Ord a=> a -> a -> a 
minimoEntre x y = if x < y
                  then x 
                  else y


--
zipMaximos :: [Int] -> [Int] -> [Int]
{-Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.
-}
zipMaximos []       []           = []
zipMaximos []       (n2:ns2)     = (n2:ns2)
zipMaximos (n1:ns1) []           = (n1:ns1)
zipMaximos (n1:ns1) (n2:ns2)     = maximoEntre n1 n2 : zipMaximos ns1 ns2 
                                  

maximoEntre :: Ord a=> a -> a -> a 
maximoEntre x y = if x > y
                  then x 
                  else y



factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1) 

cuentaRegresiva :: Int -> [Int] 
cuentaRegresiva  0 = []
cuentaRegresiva  n = n : (cuentaRegresiva (n -1)) 

repetir :: Int -> a -> [a] 
repetir 0 a = []
repetir n a = a : repetir (n-1) a

losPrimeros :: Int -> [a] -> [a] 
--Precondición: La lista no es vacía y n es menor que la longitud de la lista dada.
losPrimeros 0 []         = []
losPrimeros 0 (x:xs)     = []
losPrimeros n (x:xs)     = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a] 
sinLosPrimeros 0 a       = a
sinLosPrimeros _ []      = []  
sinLosPrimeros n (x:xs)  =  sinLosPrimeros (n-1) xs

type Nombre = String 
type Edad   = Int 


data Persona = ConsP Nombre Edad deriving Show 

tomas   = ConsP "Tomas"   24 
nicolas = ConsP "Nicolas" 25


mayoresA :: Int -> [Persona] -> [Persona] 
mayoresA  n []     = []
mayoresA  n (p:ps) = if edadEsMayorA n p
                     then p : mayoresA n ps 
                     else mayoresA n ps

edad :: Persona -> Int 
edad (ConsP n e) = e 

edadEsMayorA :: Int -> Persona -> Bool 
edadEsMayorA  x (ConsP n e) = e > x

promedioEdad :: [Persona] -> Int 
promedioEdad []     = error "Lista Vacia"
promedioEdad ps     = div(sumatoria (todasLasEdades ps)) (longitud ps) 


todasLasEdades :: [Persona] -> [Int] 
todasLasEdades []     = []
todasLasEdades (p:ps) = edad p : todasLasEdades ps


elMasViejo :: [Persona] -> Persona 
--Precondición: la lista al menos posee una persona.
elMasViejo []     = error "No hay personas en la lista"
elMasViejo [p]    = p
elMasViejo (p:ps) = laPersonaMasViejaEntre p (elMasViejo ps) 


laPersonaMasViejaEntre :: Persona -> Persona -> Persona 
laPersonaMasViejaEntre  p1 p2 = if edad p1 > edad p2 
                                then p1 
                                else p2


type NombreEntrenador  = String
type PorcentajeEnergia = Int

data TipoDePokemon = Agua | Fuego | Planta                       deriving Show
data Pokemon       = ConsPokemon TipoDePokemon PorcentajeEnergia deriving Show
data Entrenador    = ConsEntrenador NombreEntrenador [Pokemon]   deriving Show


pokemon1 :: Pokemon
pokemon1  = ConsPokemon Agua 100 

pokemon2 :: Pokemon
pokemon2  = ConsPokemon Planta 50

pokemon3 :: Pokemon
pokemon3  = ConsPokemon Fuego 50

pokemon4 :: Pokemon
pokemon4  = ConsPokemon Agua 50

pokemon5:: Pokemon 
pokemon5 = ConsPokemon Fuego 50

ash1 :: Entrenador
ash1 = ConsEntrenador "ash1" [pokemon1, pokemon2, pokemon5] 

ash2 ::Entrenador 
ash2 = ConsEntrenador "ash2" [pokemon3, pokemon4]

cantPokemon :: Entrenador -> Int
cantPokemon  (ConsEntrenador _ lsPok) = longitud lsPok

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe    tp (ConsEntrenador _ lsPok) = cantPokemonEn tp lsPok

cantPokemonEn :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonEn   tp []     = 0
cantPokemonEn   tp (p:ps) = unoSi (esDelMismoTipo (tipoDePokemon p) tp) + cantPokemonEn tp ps

tipoDePokemon :: Pokemon -> TipoDePokemon 
tipoDePokemon (ConsPokemon tp _) = tp

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool 
esDelMismoTipo   Agua Agua      = True
esDelMismoTipo   Fuego Fuego    = True
esDelMismoTipo   Planta Planta  = True
esDelMismoTipo   _  _           = False

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan    tp (ConsEntrenador _  pok1) (ConsEntrenador _ pok2) = losQueLeGananATodos (pokemonesDeTipo tp pok1) pok2 


losQueLeGananATodos :: [Pokemon] -> [Pokemon] -> Int 
losQueLeGananATodos []     _     = 0
losQueLeGananATodos (p:pks) pk2  = unoSi (superaATodos p pk2) + losQueLeGananATodos pks pk2

superaATodos :: Pokemon -> [Pokemon] -> Bool 
superaATodos  p []        = True
superaATodos  p (pk: pks) = superarA p pk && superaATodos p pks

pokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDeTipo tp []     = []
pokemonesDeTipo tp (p:ps) = if esDelMismoTipo tp (tipoDePokemon p)
                            then p : pokemonesDeTipo tp ps
                            else pokemonesDeTipo tp ps

superarA  :: Pokemon -> Pokemon -> Bool
superarA (ConsPokemon t1 _) (ConsPokemon t2 _) = puedeSuperarA t1 t2 

--Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
puedeSuperarA :: TipoDePokemon -> TipoDePokemon -> Bool 
puedeSuperarA     Agua  Fuego    = True 
puedeSuperarA     Fuego Planta   = True 
puedeSuperarA     Planta Agua    = True 
puedeSuperarA    _    _          = False 

tienePokDeTipo :: Pokemon -> TipoDePokemon -> Bool 
tienePokDeTipo  (ConsPokemon Agua _  ) Agua   = True
tienePokDeTipo  (ConsPokemon Fuego _ ) Fuego  = True 
tienePokDeTipo  (ConsPokemon Planta _) Planta = True 
tienePokDeTipo  _                      _      = False  

esMaestroPokemon :: Entrenador -> Bool
--Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon (ConsEntrenador _ [])  = False 
esMaestroPokemon (ConsEntrenador _ ps)  = (((cantPokemonEn Agua ps  ) >= 1)  && 
                                          ((cantPokemonEn Fuego ps ) >= 1 )  && 
                                          ((cantPokemonEn Planta ps) >= 1) )

type NombreProyecto = String

data Seniority = Junior | SemiSenior | Senior                           deriving Show
data Proyecto = ConsProyecto NombreProyecto                             deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol]                                        deriving Show


rol1 :: Rol
rol1 = Developer  Junior  proyecto1 

rol2 :: Rol
rol2 = Developer  Senior  proyecto2 

rol3 :: Rol
rol3 = Management Senior  proyecto3

proyecto1 :: Proyecto 
proyecto1 = ConsProyecto "Desarrollo De Paginas web"

proyecto2 :: Proyecto
proyecto2 = ConsProyecto "Desarrollo De Paginas web"

proyecto3 :: Proyecto
proyecto3 = ConsProyecto "Analista Funcional"

empresa1 :: Empresa
empresa1 = ConsEmpresa [rol1, rol2, rol3]  

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = sinProyectosRepetidos (proyectoDeRoles rs) 


sinProyectosRepetidos :: [Proyecto] -> [Proyecto] 
sinProyectosRepetidos []     = []
sinProyectosRepetidos (p:ps) = if elProyectoPertenece p ps
                               then     sinProyectosRepetidos ps 
                               else p : sinProyectosRepetidos ps 

sinProyectosRepetidosMejor :: [Proyecto] -> [Proyecto]
sinProyectosRepetidosMejor []     = []
sinProyectosRepetidosMejor (p:ps) = let ps' = sinProyectosRepetidos ps
                                     in if elProyectoPertenece p ps'
                                         then     ps'
                                         else p : ps' 


elProyectoPertenece :: Proyecto -> [Proyecto] -> Bool
elProyectoPertenece proyecto []     = False 
elProyectoPertenece proyecto (p:ps) =  sonLosMismosProyectos proyecto p || elProyectoPertenece proyecto ps

sonLosMismosProyectos :: Proyecto -> Proyecto -> Bool
sonLosMismosProyectos  (ConsProyecto p1) (ConsProyecto p2) = p1 == p2 

proyectoDeRoles :: [Rol] -> [Proyecto] 
proyectoDeRoles []     = []
proyectoDeRoles (r:rs) = (proyectoDeRol r) : (proyectoDeRoles rs) 

proyectoDeRol :: Rol -> Proyecto 
proyectoDeRol (Developer  _ proyecto)  = proyecto
proyectoDeRol (Management _ proyecto)  = proyecto 


losDevSenior :: Empresa -> [Proyecto] -> Int
--Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
--además a los proyectos dados por parámetro.
losDevSenior (ConsEmpresa [])   ps   = 0
losDevSenior (ConsEmpresa (r:rs)) ps = unoSi (esDeveloperYPerteneceALosProyectos r ps) + 
                                       losDevSenior (ConsEmpresa rs) ps

esDeveloperYPerteneceALosProyectos :: Rol -> [Proyecto] -> Bool 
esDeveloperYPerteneceALosProyectos rol ps     = ((esDeveloper rol)   &&
                                                (trabajaEnLosProyectos rol ps))

esDeveloper ::  Rol -> Bool 
esDeveloper (Developer  _ _ ) = True 
esDeveloper (Management _ _ ) = False

trabajaEnLosProyectos :: Rol -> [Proyecto] -> Bool
trabajaEnLosProyectos r []     = False
trabajaEnLosProyectos r (p:ps) = trabajaEnElProyecto r p ||  trabajaEnLosProyectos r ps

trabajaEnElProyecto :: Rol -> Proyecto -> Bool 
trabajaEnElProyecto rol proyecto = sonLosMismosProyectos (proyectoDeRol rol) proyecto

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn ps (ConsEmpresa [])     = 0
cantQueTrabajanEn ps (ConsEmpresa (r:rs)) = unoSi (trabajaEnLosProyectos r ps) + 
                                            cantQueTrabajanEn ps (ConsEmpresa rs) 

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)] 
-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
-- cantidad de personas involucradas 
asignadosPorProyecto (ConsEmpresa rs) = asignadosPorCadaProyecto rs 

asignadosPorCadaProyecto :: [Rol] -> [(Proyecto, Int)] 
asignadosPorCadaProyecto   []    = []
asignadosPorCadaProyecto  (r:rs) = consolidar (proyectoDeRol r) (asignadosPorCadaProyecto rs) 

consolidar :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)] 
consolidar   proyecto  []       = [(proyecto, 1)]
consolidar   proyecto  ((p,i):pis)   = if sonLosMismosProyectos  p proyecto 
                                  then (proyecto, i +1) : pis 
                                  else  (p,i) : consolidar proyecto pis

