longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista(xs)

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento [] elem _ = [elem]
agregaElemento (x:xs) elem bool = if bool
                                  then elem : (x:xs)
                                  else (x:xs) ++ [elem]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [x] = x
maximoLista (x:xs) = if x > maximoLista xs
                        then x
                        else maximoLista xs

indice :: [a] -> Int -> a
indice [] _ = error "Lista vacia"
indice (x:xs) elem = if elem < 0 || elem >= longitud (x:xs)  
                    then error "Índice fuera de rango"
                    else if elem == 0
                            then x
                            else indice xs (elem-1)

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y|y <- xs, y /= x]

numerosPares :: [Int] -> [Int]
numerosPares [] = []
numerosPares (x:xs) = [x | x <- (x:xs), x `mod` 2 == 0]