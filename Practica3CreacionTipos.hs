data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud(Node a lista) = 1 + longitud lista

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void y = False
estaContenido(Node x xs) elemento = if elemento == x
                                       then True
                                        else estaContenido xs elemento

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node a lista) = a : convertirALista lista

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x lista) = if estaContenido lista x
                            then conjunto lista
                            else Node x (conjunto lista)

eliminarIndice :: List a -> Int -> List a
eliminarIndice Void indice = error "No existe indice en lista vacia"
eliminarIndice(Node a lista) 0 = lista
eliminarIndice(Node a lista) indice = if indice < 0 || indice > longitud (Node a lista)-1
                                         then error "Indice fuera del rango permitido"
                                        else Node a (eliminarIndice lista(indice-1))

insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void indice x = if indice /= 0
                then error "Indice fuera del rango permitido"
                else (Node x Void)
insertarIndice (Node a lista) 0 x = Node x (Node a lista)
insertarIndice (Node a lista) indice x = if indice < 0 || indice > longitud (Node a lista)
                                            then error "Indice fuera del rango permitido"
                                            else Node a (insertarIndice lista(indice-1)x)

recorrerLista :: List a -> Int -> List a
recorrerLista Void recorrido = Void
recorrerLista lista 0 = lista
recorrerLista (Node x xs) recorrido = recorrerLista (insertarIndice xs(longitud xs) x) (recorrido - 1)