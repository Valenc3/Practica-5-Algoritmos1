--Ejercicio 1
--a
longitud :: Eq t => [t] -> Integer
longitud [] = 0
longitud (x:xs) | null xs   = 1
                | otherwise  = 1 + longitud xs


--b

ultimo :: Eq t => [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs


--c
--ChatGPT
principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs

--d

reverso :: Eq t => [t] -> [t]
reverso [x] = [x]
reverso (x:xs) = ultimo (x:xs) : reverso (principio (x:xs))


--Ejercicio 2

--1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece y [x]| y /= x = False
pertenece y (x:xs)| y == x = True
                  | otherwise = pertenece y xs


--2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [x] = True
todosIguales (x:xs)| x == ultimo (x:xs) = todosIguales (principio (x:xs))
                   | otherwise = False

--3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [x] = True
todosDistintos (x:xs)| auxDistintos (x:xs) = todosDistintos xs
                     | otherwise = False


auxDistintos :: (Eq t) => [t] -> Bool
auxDistintos [x] = True
auxDistintos (x:y:xs)| x == y = False
                     | otherwise = auxDistintos (x:xs)


--4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos (x:xs)| not (todosDistintos (x:xs)) = True
                   | otherwise = False


--5
quitar :: (Eq t) => t -> [t] -> [t]
quitar x xs| not (pertenece x xs) = xs
           | x == head xs = tail xs
           | otherwise = head xs : quitar x (tail xs)


--6
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos x xs| not (pertenece x xs) = xs
                | otherwise = quitarTodos x (quitar x xs)


--7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos (x:xs)| todosDistintos (x:xs) = x:xs
                        | otherwise = eliminarRepetidos (quitar (identifica (x:xs)) (x:xs))

identifica :: (Eq t) => [t] -> t
identifica (x:xs)| pertenece x xs = x
                 | otherwise = identifica xs

--8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] (y:ys) = True
mismosElementos (x:xs) (y:ys)| length (x:xs) < length (y:ys) = mismosElementos (y:ys) (x:xs)
                             | pertenece x (y:ys) = mismosElementos xs (y:ys)
                             | otherwise = False


--9
capicua :: (Eq t) => [t] -> Bool
capicua (x:xs)| reverso (x:xs) == (x:xs) = True
              | otherwise = False


--Ejercicio 3

--1
--sumatoria = sum 
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--2
--productoria = product
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs


--3
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs)| x > y = maximo (x:xs)
               | otherwise = maximo (y:xs)


--4
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = (x + n) : sumarN n xs


--5
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

--6
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo (x:xs) = sumarN (ultimo (x:xs)) (x:xs)


--7
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs)| even x = x : pares xs
            | otherwise = pares xs


--8
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs)| mod x n == 0 = x : multiplosDeN n xs
                     | otherwise = multiplosDeN n xs


--9 
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = minimo [x] : ordenar (quitar (minimo [x]) [x])


minimo :: [Integer] -> Integer
minimo [x] = x
minimo (x:y:xs)| x > y = minimo (y:xs)
               | otherwise = minimo (x:xs)

--Ejercicio 5
sumaAcumulada1 :: (Num t) => [t] -> [t]
sumaAcumulada1 [x] = [x]
sumaAcumulada1 (x:xs) = sum (x:xs) :sumaAcumulada1 (principio (x:xs))

sumaAcumulada :: (Num t, Eq t) => [t] -> [t]
sumaAcumulada (x:xs) = reverso (sumaAcumulada1 (x:xs))
--sumaAcumulada (x:xs) = x :sumaAcumulada (sumarN x xs)

--b 
descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos = map listaDivisores
--descomponerEnPrimos :: [Integer] -> [[Integer]]
--descomponerEnPrimos [] = []
--descomponerEnPrimos (x:xs) = listaDivisores x : descomponerEnPrimos xs  




listaDivisores :: Integer -> [Integer]
listaDivisores x| x == 1 = [1]
              | otherwise = divisionLista [1..x] x


divisionLista :: [Integer] -> Integer -> [Integer]
divisionLista [] n = []
divisionLista (1:xs) n = divisionLista xs n
divisionLista (x:xs) n | mod n x == 0 = x : divisionLista (x:xs) (div n x)
                       | otherwise = divisionLista xs n
