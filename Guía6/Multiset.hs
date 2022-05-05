module Multiset(Multiset) where 


{- 
Inv. Representación: 
El int del multiset representa la cantidad de apariciones de un elemento en la lista
-}
data Multiset a = MS [a] Int deriving Show

{-
instance Show a => Show (Multiset a) where 
         show (MS xs n) = "{" ++ mostrar xs ++ "}"

mostrar []  _    = "" 0
mostrar [x] n    = show x n
mostrar(x:xs) n  =  show x n ++ "," ++ mostrar xs 
-}

emptyMS :: Multiset a 
emptyMS = MS [] 0