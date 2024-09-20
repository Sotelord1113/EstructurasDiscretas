longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista(xs)

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento (x:xs) a agregar = if agregar==True 
                                then a : (x:xs)
                                else (x:xs)++[a]
agregaElemento [] a agregar = if agregar==True 
                                then a : []
                                else []++[a]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [x] = x
maximoLista (x:xs) = max x (maximoLista xs)

indice :: [a] -> Int -> a
indice [] _ = error "Lista vacia"
indice (x:xs) y = if y<0 || y>longitud (x:xs)  
                    then error "Índice fuera de rango"
                    else if y == 0
                            then x
                            else indice xs (y-1)

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y|y <- xs, y /= x]

numerosPares :: [Int] -> [Int]
numerosPares [] = []
numerosPares (x:xs) = if x `mod` 2 ==0
                    then x: numerosPares xs
                    else numerosPares xs