{-

1. Definir las siguientes funciones recursivas:
   a) borrarUltimo que dada una lista borra el último elemento de la lista. 
      No utilizar reverse, ni tail.
   b) collect :: [(k, v)] -> [(k, [v])] toma una lista de pares (clave, valor) 
      y asocia cada clave única con todos los valores con los que estaba 
      apareada originalmente.
      Por ejemplo: collect [('a', 4), ('b', 1), ('a', 5), ('c', 8)] 
                 = [('a', [4, 5]), ('b', [1]), ('c', [8])]
   c) serie que se comporta de la siguiente manera: 
      serie [1, 2, 3] = [[], [1], [1, 2], [1, 2, 3]]
      Dar su tipo más general.
   d) ror que dada una lista xs y un entero n, tal que n <= length xs, rota 
      los primeros n elementos de xs a la derecha: 
      ror 3 [1, 2, 3, 4, 5] = [4, 5, 1, 2, 3]. 
      Definir una versión recursiva de ror, sin usar drop, take ni tail.
   e) upto que dados dos números enteros n y m devuelve la lista [n, n+1, n+2, ..., m] 
      en caso que n <= m y la lista vacía en otro caso.
   f) eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo 
      cada elemento tantas veces como indica su posición.
      Por ejemplo: eco "hola" = "hoolllaaaa"

2. Definir usando listas por comprensión:
   a) divisors, que dado un entero positivo x devuelve la lista de los divisores 
      de x (y la lista vacía si el entero no es positivo).
   b) matches, que dados un entero x y una lista de enteros descarta de la lista 
      los elementos distintos a x.
   c) cuadrupla, que dados cuatro enteros a, b, c y d tales que 0 < a, b, c, d < 100, 
      devuelve las cuadruplas (a, b, c, d) que cumplen a^2 + b^2 = c^2 + d^2.
   d) unique, que dada una lista xs de enteros, devuelve la lista con los 
      elementos no repetidos de xs.
   e) cambios, que dada una lista, devuelve la lista de los índices en que la 
      lista cambia. Es decir, dada la lista s retorna la lista con los i 
      tal que s_i /= s_(i+1).
      Ejemplo: cambios [1, 1, 1, 3, 3, 1, 1] = [2, 4]
   f) expandir, que reemplace en una lista de números positivos cada número n 
      por n copias de sí mismo.
      Ejemplo: expandir [3, 4, 2] = [3, 3, 3, 4, 4, 4, 2, 2]

3. El producto escalar de dos listas de enteros de igual longitud es la suma 
   de los productos de los elementos sucesivos (misma posición) de ambas listas. 
   Definir una función 'scalarProduct' que devuelva el producto escalar de dos listas.
   Sugerencia: Usar las funciones 'zip' y 'sum'.

4. Dar dos ejemplos de funciones que tengan los siguientes tipos:
   a) (Int -> Int) -> Int
   b) Int -> (Int -> Int)
   c) (Int -> Int) -> (Int -> Int)
   d) Int -> Bool
   e) Bool -> (Bool -> Bool)
   f) (Int, Char) -> Bool
   g) (Int, Int) -> Int
   h) Int -> (Int, Int)
   i) a -> Bool
   j) a -> a
   k) Int -> (Int -> Bool) -> [Int]
   l) [[a]] -> (a -> Bool) -> [a]

5. Reescribir cada una de las siguientes definiciones sin usar let, where o if:
   a) f x = let (y, z) = (x, x) in y
   b) greater (x, y) = if x > y then True else False
   c) f (x, y) = let z = x + y in g (z, y) where g (a, b) = a - b

6. Suponiendo que f y g tienen los siguientes tipos:
      f :: c -> d
      g :: a -> b -> c
   y sea h definida como:
      h x y = f (g x y)
   Determinar el tipo de h e indicar cuáles de las siguientes definiciones de h son equivalentes a la dada:
      - h = f . g
      - h x = f . (g x)
      - h x y = (f . g) x y
   ¿Cuál es el tipo de la función (.)?

   7. Definir usando funciones de alto orden (map, filter, foldr).
a) filterEven’ :: [Int] → [Int], que dada una lista de enteros, devuelve una lista donde los unicos elementos son los numeros pares de la lista de entrada.
b) and’ :: [Bool] → Bool, que dado una lista de valores Booleanos devuelve True si todos los elemento de la lista son True, y False en caso contrario.
c) longitudes :: [[a]] → [Int], que dada una lista de listas devuelve una lista de las longitudes de cada lista. Ejemplo: longitudes [[1,2,3], [4,5], []] = [3,2,0]
d) unzip :: [(a, b)] → ([a], [b]), que dada una lista de tuplas xs retorna una tupla de listas donde cada una corresponde a los primeros y segundos elementos de los pares respectivamente. Ej. unzip [(′𝑎′, 1), (′𝑧′, 7), (′ℎ′, 9)] = (”𝑎𝑧ℎ”, [1, 7, 9])
e) pair2List :: (a, [b]) → [(a, b)] que dado un par formado por un valor 𝑥 y una lista 𝑥𝑠 convierta a la lista xs en una lista de pares, formada con los elementos de 𝑥𝑠 y 𝑥. Ejemplo: pair2List (𝑥, [𝑦1, 𝑦2, 𝑦3]) = [(𝑥, 𝑦1), (𝑥, 𝑦2), (𝑥, 𝑦3)]
f) maxSec :: [(Int, Int)] → (Int, Int), que dada una lista de pares de naturales que represente a una lista de segmentos de la recta, calcule el segmento mas largo de la misma. Ejemplo: maxSec [(1, 2), (0, 7), (4, 6)] = (0, 7) Puede definir una funcion auxiliar maxL :: (Int, Int) → (Int, Int) → (Int, Int), que dados dos pares de naturales que representan a dos segmentos de la recta, devuelva el segmento cuya longitud sea maxima. Ejemplo maxL (1, 2) (0, 7) = (0, 7)

-}

-- 1)
-- a)
borrarUltimo :: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

-- b)

-- c)
serie :: [a] -> [[a]]
serie [] = [[]]
serie (x:xs) = [] : map (x:) (serie xs)

-- d)
ror :: Int -> [a] -> [a]
ror _ [] = []
ror 0 xs = xs
ror n (x:xs)
    | n <= length xs = ror (n - 1) (xs ++ [x])

-- e)
upto :: Int -> Int -> [Int]
upto n m
    | n <= m = n : upto (n + 1) m
    | otherwise = []

-- f)
eco :: String -> String
eco xs = concat [replicate i x | (x, i) <- zip xs [1..]]

-- 2)
-- a)
divisors :: Int -> [Int]
divisors n
    | n > 0 = [x | x <- [1..n], mod n x == 0]
    | otherwise = []

-- b)
matches :: Int -> [Int] -> [Int]
matches _ [] = []
matches n (x:xs)
    | n /= x = matches n xs
    | otherwise = x : (matches n xs)

-- c)
cuadrupla :: [(Int, Int, Int, Int)]
cuadrupla = [(a, b, c, d) | a <- [1..99], b <- [1..99], c <- [1..99], d <- [1..99], a ^ 2 + b ^ 2 == c ^ 2 + d ^ 2]

-- d)
-- unique :: [Int] -> [Int]
-- unique [] = []
-- unique (x:xs)
--    | notElem x xs = x : unique xs
--    | otherwise = unique xs

unique :: [Int] -> [Int]
unique xs = [x | x <- xs, length [y | y <- xs, y == x] == 1]

-- e)
cambios :: Eq a => [a] -> [Int]
cambios xs = [i | i <- [0..length xs - 2], xs !! i /= xs !! (i + 1)]

-- f)
expandir :: [Int] -> [Int]
expandir [] = []
expandir (x:xs) = replicate x x ++ expandir xs

-- 3)
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x,y) <- zip xs ys]

-- 5)
-- a)
fa :: a -> a
fa x = x

-- b)
greater :: (Int, Int) -> Bool
greater (x, y)
    | x > y = True
    | otherwise = False

-- c)
fc :: (Int, Int) -> Int
fc (x, y) = x

-- 7)
-- a)
filterEven' :: [Int] -> [Int]
filterEven' xs = filter even xs

-- b)
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- c)
longitudes :: [[a]] -> [Int]
longitudes xss = map length xss

-- d)
unzip' :: [(a, b)] -> ([a], [b])
unzip' xss = (map fst xss, map snd xss)

-- e)
pair2List :: (a, [b]) -> [(a, b)]
pair2List (x, xs) = map (\y -> (x, y)) xs
