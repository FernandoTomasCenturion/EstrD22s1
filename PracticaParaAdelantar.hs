singularSi:: a -> Bool -> [a] 
singularSi x True  = [x]
singularSi x False = []


pertenece :: Eq a => a -> [a] -> Bool 
pertenece x []     =  False
pertenece x (y:ys) =  (x == y) || pertenece x ys

type Presa       = String 
type Territorio  = String 
type Nombre      = String


data Lobo   =    Cazador Nombre [Presa] Lobo Lobo Lobo
               | Explorador Nombre [Territorio] Lobo Lobo 
               | Cria Nombre  deriving Show              

data Manada = M Lobo deriving (Show)

t1 = "Berazategui" 

--Punto 1 
manada = M (Cazador "Tomas" ["Presa1", "Presa2", "Presa3"]
           (Explorador "Fernando" [t1]
           (Cria "cria1")
           (Cria "cria2"))
           (Explorador "Franco" [t1]
           (Cria "cria3")
           (Cria "cria4"))
           (Cria "cria5"))

buenaCaza :: Manada -> Bool 
buenaCaza (M l) = cantDePresas l > cantDeCrias l 


cantDePresas :: Lobo -> Int 
cantDePresas (Cria       n            ) = 0
cantDePresas (Explorador n ts l1 l2   ) = cantDePresas l1 + cantDePresas l2  
cantDePresas (Cazador    n ps l1 l2 l3) = length ps + cantDePresas l1 + cantDePresas l2 + cantDePresas l3 

cantDeCrias :: Lobo -> Int 
cantDeCrias (Cria       n            ) = 1
cantDeCrias (Explorador n ts l1 l2   ) = cantDeCrias l1 + cantDeCrias l2 
cantDeCrias (Cazador    n ps l1 l2 l3) = cantDeCrias l1 + cantDeCrias l2 + cantDeCrias l3


elAlfa :: Manada -> (Nombre, Int) 
elAlfa (M l) = elAlfaL l 


elAlfaL :: Lobo -> (Nombre, Int) 
elAlfaL (Cria       n            ) = (n, 0)
elAlfaL (Explorador n ts l1 l2   ) = elMayorCazador (n, 0        ) (elMayorCazador (elAlfaL l1) 
                                                                                   (elAlfaL l2))
elAlfaL (Cazador    n ps l1 l2 l3) = elMayorCazador (n, length ps) (elMayorCazador (elAlfaL l1) 
                                                                   (elMayorCazador (elAlfaL l2)
                                                                                   (elAlfaL l3)))


elMayorCazador :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int) 
elMayorCazador (n1, x1) (n2, x2) = if snd(n1, x1) > snd(n1, x2) 
                                    then (n1, x1)
                                    else (n2, x2)


losQueExploraron :: Territorio -> Manada -> [Nombre] 
losQueExploraron t (M l) = losQueExploraronL t l 


losQueExploraronL :: Territorio -> Lobo -> [Nombre] 
losQueExploraronL t (Cria       n            ) = []
losQueExploraronL t (Explorador n ts l1 l2   ) = singularSi n (pertenece t ts) ++ 
                                                 losQueExploraronL t l1 ++ 
                                                 losQueExploraronL t l2 
losQueExploraronL t (Cazador    n ps l1 l2 l3) = losQueExploraronL t l1 ++ 
                                                 losQueExploraronL t l2 ++ 
                                                 losQueExploraronL t l3 

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioL l 


exploradoresPorTerritorioL :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioL (Cria       n            ) = []
exploradoresPorTerritorioL (Explorador n ts l1 l2   ) = agregarLoboEnTerritorio n ts (
                                                        unionDeTuplasDeTerrotoriosSinRepetidos
                                                        (exploradoresPorTerritorioL l1)
                                                        (exploradoresPorTerritorioL l2))

exploradoresPorTerritorioL (Cazador    n ts l1 l2 l3) = unionDeTuplasDeTerrotoriosSinRepetidos (exploradoresPorTerritorioL l1) 
                                                        (unionDeTuplasDeTerrotoriosSinRepetidos
                                                        (exploradoresPorTerritorioL l2)
                                                        (exploradoresPorTerritorioL l3))


agregarLoboEnTerritorio :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarLoboEnTerritorio n []     tns = tns 
agregarLoboEnTerritorio n (t:ts) tns = agregarLoboT n t (agregarLoboEnTerritorio n ts tns)


agregarLoboT :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
agregarLoboT nombre territorio []       = [(territorio, [nombre])]
agregarLoboT nombre territorio (tn:tns) = let (t, ns) = tn 
                                          in if (territorio == t) 
                                            then (t, agregarSiNoAparece nombre ns) : tns 
                                            else tn : agregarLoboT nombre territorio tns 


agregarSiNoAparece :: Eq a => a -> [a] -> [a] 
agregarSiNoAparece  x []     = []
agregarSiNoAparece  x (y:ys) = if x == y 
                               then (y:ys) 
                               else y : agregarSiNoAparece x ys 

unionDeTuplasDeTerrotoriosSinRepetidos :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
unionDeTuplasDeTerrotoriosSinRepetidos []        tns2   = tns2
unionDeTuplasDeTerrotoriosSinRepetidos (tn:tns1) tns2   = agregarOUnirTerritorio tn tns2 

agregarOUnirTerritorio :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])]  -> [(Territorio, [Nombre])]  
agregarOUnirTerritorio tn1 []          = [tn1]
agregarOUnirTerritorio tn1 (tn2: tns2) = let (t1, l1) = tn1 
                                             (t2, l2) = tn2 
                                          in 
                                          if(t1 == t2) 
                                           then(t1, unirSinRepetidos l1 l2) : tns2 
                                           else tn2 : agregarOUnirTerritorio tn1 tns2 

unirSinRepetidos :: Eq a => [a] -> [a] -> [a] 
unirSinRepetidos []     ys = ys
unirSinRepetidos (x:xs) ys = agregarSiNoAparece x (unirSinRepetidos xs ys)
