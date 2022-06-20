data RAList a = MkR Int (Map Int a) (Heap a)

{-Inv. Rep: Con la raList = siendo MkR n m h, 
    *n >= 0.
    *lenght (domM m) = n. O sea, la longitud del map debe ser igual a n. 
    *Todos los elementos de m deben estar en h y viceversa.
-}