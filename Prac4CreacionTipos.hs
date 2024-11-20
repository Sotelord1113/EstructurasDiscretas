data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud ArbolVacio = 0
longitud (Raiz _ izq der) = 1 + longitud izq + longitud der

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad ArbolVacio = 0
profundidad (Raiz _ izq der) = 1 + max (profundidad izq) (profundidad der)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ izq der) = ancho izq + ancho der

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz x izq der) InOrder = recorrido izq InOrder ++ [x] ++ recorrido der InOrder
recorrido (Raiz x izq der) PreOrder = [x] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz x izq der) PosOrder = recorrido izq PosOrder ++ recorrido der PosOrder ++ [x]

-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz x izq der) = [x]:combinarNiveles(niveles izq) (niveles der)

combinarNiveles:: [[a]] -> [[a]] -> [[a]]
combinarNiveles xs [] = xs
combinarNiveles [] xs = xs
combinarNiveles (x:xs) (y:ys) = (x++y):combinarNiveles xs ys

-------------------- EJERCICIO 6 --------------------
minimo :: Arbol a -> a 
minimo ArbolVacio = error "Árbol vacío"
minimo (Raiz x ArbolVacio _) = x
minimo (Raiz _ izq _) = minimo izq

-------------------- EJERCICIO 7 --------------------
maximo :: Arbol a -> a 
maximo ArbolVacio = error "Árbol vacío"
maximo (Raiz x _ ArbolVacio) = x
maximo (Raiz _ _ der) = maximo der

-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a 
eliminar ArbolVacio _ = error "El árbol está vacío"
eliminar (Raiz x ArbolVacio der) n
    | n == x = der  
eliminar (Raiz x izq ArbolVacio) n
    | n == x = izq  
eliminar (Raiz x izq der) n
    | n < x = Raiz x (eliminar izq n) der  
    | n > x = Raiz x izq (eliminar der n)  
    | n == x = Raiz (maximo izq) (eliminar izq (maximo izq)) der  