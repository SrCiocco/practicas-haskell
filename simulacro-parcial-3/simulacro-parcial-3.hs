{-
1. Dar ejemplos de funciones que tengan los siguientes tipos:
a) (Int -> Bool) -> Int -> [Int]
b) [(a, b)] -> (b -> c) -> [c]

2. Los "sufijos" de una lista son todas las sublistas finales que se obtienen al ir descartando
	elementos desde el principio de la lista original hasta llegar a la lista vacía.

	Por ejemplo:
		sufijos [1, 2, 3] = [[1, 2, 3], [2, 3], [3], []]

a) Definir una función recursiva, drop' :: Int -> [a] -> [a], que dado un número entero n y una lista,
	elimine los primeros n elementos de la lista.

	Por ejemplo:
		drop' 2 [1, 2, 3, 4] = [3, 4]

b) Definir una función recursiva, sufijos :: [a] -> [[a]], que dada una lista xs devuelva
	la lista con todos sus sufijos.

	Por ejemplo:
		sufijos [1, 2, 3] = [[1, 2, 3], [2, 3], [3], []]

c) Definir una función usando listas por comprensión, paresEnSublistas :: [[Int]] -> [Int],
	que dada una lista de listas de enteros devuelva una lista unidimensional con
	todos los números pares que aparecen en ellas. Puede usar la función even predefinida.

	Por ejemplo:
		paresEnSublistas [[1, 2, 3], [2, 3], [3], []] = [2, 2]

d) Para reducir la lista de listas obtenida a un solo valor, definir usando la función
	de alto orden foldr, sumaTotal :: [[Int]] -> Int, que reciba una lista de
	listas de enteros y calcule la suma de todos los números contenidos en
	todas las sublistas. Puede usar la función sum predefinida.

	Por ejemplo:
		sumaTotal [[1, 2, 3], [2, 3], [3], []] = 14
-}

-- 1)
-- a)
ejemploA :: (Int -> Bool) -> Int -> [Int]
ejemploA f x
    | f x = [x]
    | otherwise = []

-- b)
ejemploB :: [(a, b)] -> (b -> c) -> [c]
ejemploB xs f = map (\(_,y) -> f y) xs

-- 2)
-- a)
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (x:xs) = drop' (n - 1) xs

-- b)
sufijos :: [a] -> [[a]]
sufijos [] = [[]]
sufijos (x:xs) = (x:xs):sufijos xs

-- c)
paresEnSublistas :: [[Int]] -> [Int]
paresEnSublistas [] = []
paresEnSublistas xss = [x | xs <- xss, x <- xs, even x]

-- d)
sumaTotal :: [[Int]] -> Int
sumaTotal xss = foldr (\xs acc -> sum xs + acc) 0 xss
