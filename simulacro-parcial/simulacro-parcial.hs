{-
1. Dar ejemplos de funciones que tengan los siguientes tipos:
a) (Int → Int) → Int
b) [[a]] → (a → Bool) → [a]

2. La "serie" de una lista es una secuencia que agrupa las sublistas iniciales progresivas.
	Se empieza con la lista vacía, y en los pasos siguientes se van agregando
	los elementos de la lista original de forma secuencial hasta obtener
	la lista completa.

	Por ejemplo: 
	serie [1, 2, 3] = [[], [1], [1, 2], [1, 2, 3]]

a) Definir una función recursiva, take' :: Int → [a] → [a], que dado un número entero
	n y una lista, devuelve los primeros n elementos de la lista.

	Por ejemplo: take' 2 [1, 2, 3] = [1, 2]

b) Definir una función recursiva, serie :: [a] → [[a]], que dada una lista xs
	devuelva su serie correspondiente. Puede usar la función del apartado anterior 
	y la función length predefinida.

c) Definir una función usando listas por comprensión, longitudes :: [[a]] → [Int]
	que dada una lista de listas devuelve una lista de las longitudes de cada lista.

	Por ejemplo: longitudes [[1, 2, 3], [4, 5], []] = [3, 2, 0]

d) Para reducir la lista de listas obtenida a un solo valor, definir usando
	la función de alto orden foldr, aplanar :: [[a]] → [a], que reciba la serie generada
	y concatene todas sus sublistas en una sola lista unidimensional.
	Tenga en cuenta que foldr opera reemplazando cada constructor de lista (:)
	por la operación provista y el final de la lista ([]) por el valor base.

	Por ejemplo: aplanar [[], [1], [1, 2]] = [1, 1, 2]
-}

-- 1)
-- a)
ejemploA :: (Int -> Int) -> Int
ejemploA f = f 5

-- b)
ejemploB :: [[a]] -> (a -> Bool) -> [a]
ejemploB xss f = filter f (head xss)

-- 2)
-- a)
take' :: Int -> [a] -> [a]
take' 0 xs = []
take' _ [] = []
take' n (x:xs) = x:take' (n - 1) xs

-- b)
serie :: [a] -> [[a]]
serie [] = [[]]
serie xs = serie (take' (length xs - 1) xs) ++ [xs]

-- c)
longitudes :: [[a]] -> [Int]
longitudes xss = [length x | x <- xss]

-- d)
aplanar :: [[a]] -> [a]
aplanar xss = foldr (++) [] xss
