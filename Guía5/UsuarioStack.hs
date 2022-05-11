import Stack 

--O(n) siendo n la cantidad de elementos de la lista dada.
apilar :: [a] -> Stack a
apilar     [] = emptyS
apilar (x:xs) = push x (apilar xs) 

--O(n) siendo n la cantidad de elementos de la pila dada.
desapilar :: Stack a -> [a] 
desapilar stack = if isEmptyS stack 
                  then []
                  else top stack : desapilar (pop stack)

--O(n) siendo n el nbúmero dado como parámetro.
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 x stack = push x stack 
insertarEnPos n x stack = insertarEnPos (n-1) x (pop stack)