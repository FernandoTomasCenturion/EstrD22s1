data Lynder = L (Matrix Lat Lon [Lyndo])
              (Map Lyndo (Lat, Lon))

{-
    Inv. rep: * todos los lyndos del map DEBEN estar en alguna lista de la matrix, y vicerversa.
              * si el lyndo esta en el map que está en una posición aparece en la lista de esa posición, y vicerversa.
              * las listas de la matrix no tienen repetidos.
-}              

numLyndos :: Lynder -> Int 
numLyndos (L mx mp) = sizeM mp 
--Justificación de costo: sizeM es 0(1), y los accesos por pm también.

register :: Lat -> Lon -> Lyndo -> Lynder -> Lynder
register x y ly (L mx mp) =  case lookUpM ly mp of
                               Nothing   ->  L(agregarEn ly x y mx)                                 
                               Just pos  ->  (agregarEn ly x y(borrarDe y pos mx)) 
                                             (assocM ly (x,y) mp)
--O(Log x + Log Y + LP+ Log L)


agregarEn:: Lyndo -> Lat -> Lon -> Matrix 
agregarEn ly x y mx = case (getM mx x y) of
                      Nothing  -> setM mx x y [ly]
                      Just lys -> setM mx x y (ly:lys)  
--O(Log x + Log y)
borrarDe:: Lyndo -> (X, Y) -> Matrix -> Matrix
borrarDe ly (x,y) mx = case (getM mx x y) of 
                       Nothing  -> mx 
                       Just lys -> setM mx x y (delete ly lys) --O(lp)
--O(Log x + log y + LP)                       


search :: Lyndo -> Distance -> Lynder -> [Lyndo] 
search ly d (L mx mp) = let Just posl = lookUpM ly mp 
                        in filtrarLosQueEstanAMenosDe d posL (keys mp) mp 

--O(L* Log L)
filtrarLosQueEstanAMenosDe :: 
filtrarLosQueEstanAMenosDe []       mp      = []    
filtrarLosQueEstanAMenosDe posl (ly:lys) mp = let Just pos = lookUpM ly mp 
                                          in singularSi(distance posL pos < d) ly : filtrarLosQueEstanAMenosDe posL lys mp

match :: Lyndo -> Lyndo -> Lynder -> Lynder  
match ly1 ly2 (L mx mp) = let Just pos1 = lookUpM ly1 mp 
                              Just pos2 = lookUpM ly2 mp 
                            L(borrarDe ly1 pos1(borrarDe ly pos2))
                            (deleteM ly(deleteM ly mp))


data Matrix iX iY v = Mx (Map iX (Map iY v))
{-
    Inv. Rep: Ninguno.
-}

setM :: Matrix Ix Iy V -> Ix -> Iy -> v -> Matrix iX iY v 
setM (Mx mx) x y v = case lookUpM x mx of 
                     Nothing  -> Mx (assocM x (assocM y v emptyM) mx)
                     Just my  -> Mx(assocM x (assocM y v my) mx)
--O(Log IX + Log IY)

getM :: Matrix Ix Iy V -> Ix -> Iy -> Maybe v 
--O(Log Ix + Log Iy)
getM (Mx mx) x y = case lookUpM x mx of 
                  Nothing -> Nothing 
                  Just my -> lookUpM y my 
