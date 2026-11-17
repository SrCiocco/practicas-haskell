{-
1. Dar ejemplos de funciones que tengan los siguientes tipos:
a) (a -> b) -> [a] -> b
b) [Int] -> (Int -> Bool) -> Bool

2. La "pirámide de sumas" de una lista de enteros se construye partiendo de una lista inicial
	en la cima, y formando cada fila inferior sumando cada par de elementos adyacentes
	de la fila anterior, hasta llegar a una fila de un único elemento.

	Por ejemplo, para [1, 2, 3, 4]:

                [1, 2, 3, 4]
                 [3, 5, 7]
                   [8, 12]
		    [20]

a) Definir una función recursiva, reducirFila :: [Int] -> [Int], que dada una fila de la pirámide,
	devuelva la fila siguiente (la inferior) formada por las sumas de los pares adyacentes.

	Por ejemplo: reducirFila [1, 2, 3, 4] = [3, 5, 7] ya que [3, 5, 7] = [1+2, 2+3, 3+4]

b) Definir una función recursiva, piramide :: [Int] -> [[Int]], que dada una lista de enteros
	devuelva la lista con todas las filas de su pirámide. 
	(Si la lista tiene un solo elemento, devuelve una lista con esa única fila;
	si es vacía, devuelve []). Puede usar la función del apartado anterior.

	Por ejemplo: piramide [1, 2, 3, 4] = [[1, 2, 3, 4], [3, 5, 7], [8, 12], [20]]

c) Definir una función usando listas por comprensión, mayoresA :: Int -> [[Int]] -> [Int],
	que dado un número entero n y una lista de listas, devuelva una lista unidimensional
	con todos los números que sean estrictamente mayores a n.

	Por ejemplo: mayoresA 5 [[1, 2, 3, 4], [3, 5, 7], [8, 12], [20]] = [7, 8, 12, 20]

d) Para reducir la pirámide obtenida a un solo valor, definir usando la función de alto orden 
	foldr, productoPuntas :: [[Int]] -> Int, que reciba una lista de listas no vacías
	y calcule el producto del primer elemento de cada sublista. 
	Puede usar la función head predefinida.

	Por ejemplo: productoPuntas [[1, 2, 3, 4], [3, 5, 7], [8, 12], [20]] = 480
-}

-- 1)
-- a)
ejemploA :: (a -> b) -> [a] -> b
ejemploA f xs = f (head xs)

-- b)
ejemploB :: [Int] -> (Int -> Bool) -> Bool
ejemploB xs f = f (head xs)

-- 2)
-- a)
reducirFila :: [Int] -> [Int]
reducirFila [] = []
reducirFila [_] = []
reducirFila (x:y:xs) = (x + y):reducirFila (y:xs)

-- b)
piramide :: [Int] -> [[Int]]
piramide [] = []
piramide [x] = [[x]]
piramide xs = xs:piramide (reducirFila xs)

-- c)
mayoresA :: Int -> [[Int]] -> [Int]
mayoresA n xss = [x | xs <- xss, x <- xs, x > 5]

-- d)
productoPuntas :: [[Int]] -> Int
productoPuntas xss = foldr (\xs acc -> head xs * acc) 1 xss
