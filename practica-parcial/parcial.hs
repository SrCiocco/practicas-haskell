{-
1. Dar ejemplos de funciones que tengan los siguientes tipos:
a) [ Int] → (Bool → Int) → Int
b) a → (a → [ a ] → b) → b

2. El triángulo de Pascal es un triángulo de números.
 
Se empieza con un 1 en la primera fila, y en las filas siguientes se van colocando números de forma
	que cada uno de ellos sea la suma de los dos números que tiene encima. 
En el caso de los extremos de la fila, ya que no tienen dos números encima, se completan con un 1.
 
                                            1
                                           1 1
                                          1 2 1
                                         1 3 3 1
                                        1 4 6 4 1
                                     1 5 10 10 5 1
                                            ...

a) Definir una función usando recursión, sigFila :: [Int] → [Int] que dada una lista que representa la n-ésima
	fila del triangulo, devuelva una lista que sea la fila siguiente (la inferior).
Por ejemplo: sigFila [1, 2, 1] = [1, 3, 3, 1] ya que [1, 3, 3, 1] = [1, (1+2), (2+1), 1]

b) Definir una función recursiva, pascal :: Int → [Int] que dado un número n, devuelva una lista que sea la
	n-ésima fila del triangulo (consideramos que la primer fila es la fila 0).
	Puede usar la función del apartado anterior.

	Por ejemplo: pascal 5 = [1, 5, 10, 10, 5, 1]a

c) Definir una función usando listas por comprensión, impares :: Int → Int que un número n devuelve
	la cantidad de numeros impares que hay en las primeras n filas. Puede considerar 
	las funciones anteriores y la función length ya definidas.

	Por ejemplo: impares 3 = 5 ya que las primeras 3 filas son [1], [1, 1] y [1, 2, 1].

d) Una propiedad interesante que tiene el triángulo de Pascal es que la suma de la n-ésima fila es igual a 2𝑛.
Por ejemplo la suma de la tercera fila [1, 3, 3, 1] es 8 = 23
Usando esta propiedad, definir una función usando funciones de alto orden, potencia :: Int → Int que
dado un número n devuelve 2 𝑛 . Puede considerar las funciones anteriores ya definidas.
-}

-- 1)
-- a)
ejemploA :: [Int] -> (Bool -> Int) -> Int
ejemploA xs f = head xs + 1 + f False

-- b)
ejemploB :: a -> (a -> [a] -> b) -> b
ejemploB x f = f x [x]

-- 2)
-- a)
sigFila :: [Int] -> [Int]
sigFila xs = 1:sumaPares xs ++ [1]
    where
    sumaPares :: [Int] -> [Int]
    sumaPares (x:y:xs) = (x + y):sumaPares (y:xs)
    sumaPares _ = []

-- b)
pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = sigFila (pascal (n - 1))

-- c)
impares :: Int -> Int
impares n = length [x | i <- [0 .. (n - 1)], x <- pascal i, odd x]

-- d)
potencia :: Int -> Int
potencia n = foldr (+) 0 (pascal n)
