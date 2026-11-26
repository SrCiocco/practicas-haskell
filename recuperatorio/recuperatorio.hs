{-
1) Definir una función expandir, que reemplace en una lista de números positivos
	cada número n por n copias de sí mismo.

	Ejemplo: expandir [3, 4, 2] = [3, 3, 3, 4, 4, 4, 4, 2, 2].
	Definir usando recursión (se pueden definir funciones auxiliares recursivas si se necesita)

2) Definir la función expandirList similar a la anterior pero usando listas por comprensión. No usar funciones auxiliares

3) Definir usando funciones de alto orden (map, filter, foldr),
	unzip :: [(a, b)] -> ([a], [b]), que dada una lista de tuplas xs
	retorna una tupla de listas donde cada una corresponde a los primeros y
	segundos elementos de los pares respectivamente. 

	Ejemplo: unzip [('a', 1), ('z', 7), ('h', 9)] = ("azh", [1, 7, 9]).
	Definir las funciones auxiliares que vaya a usar en las aplicaciones de las funciones de alto orden.
-}

-- 1)
auxCopiar :: Int -> Int -> [Int]
auxCopiar n _ | n <= 0 = []
auxCopiar n x = x:auxCopiar (n - 1) x

expandir :: [Int] -> [Int]
expandir [] = []
expandir (x:xs) = auxCopiar x x ++ expandir xs

-- 2)
expandirList :: [Int] -> [Int]
expandirList xs = [x | x <- xs, y <- [1 .. x]]

-- 3)
unzip' :: [(a, b)] -> ([a], [b])
unzip' xs = (map fst xs, map snd xs)
