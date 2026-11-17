{-
1. indicesIguales: Devuelve los índices donde un elemento es igual al siguiente.
   - QUÉ USAR: Listas por comprensión, `[0 .. length xs - 2]`, `!!`.

2. repetidos: Devuelve los elementos que aparecen más de una vez.
   - QUÉ USAR: Listas por comprensión anidadas, `x <- xs`, `length` en la guarda.

3. posicionesPares: Devuelve los elementos ubicados en índices pares (0, 2, 4...).
   - QUÉ USAR: Listas por comprensión, `[0 .. length xs - 1]`, `even`, `!!` (antes del `|`).

4. diferencia: Devuelve los elementos de 'xs' que no están en 'ys'.
   - QUÉ USAR: `x <- xs`, sub-lista iterando sobre `ys` en la guarda, `length`.

5. indicesPicos: Devuelve los índices de los elementos que son estrictamente
	mayores a sus vecinos inmediatos (izquierdo y derecho).
   - QUÉ USAR: `[1 .. length xs - 2]`, `!!` evaluando `i`, `i-1` e `i+1`.
-}

-- 1.
indicesIguales :: Eq a => [a] -> [Int]
indicesIguales xs = [i | i <- [0 .. length xs - 2],
    xs !! i == xs !! (i + 1)]

-- 2.
repetidos :: Eq a => [a] -> [a]
repetidos xs = [x | x <- xs, length [y | y <- xs, y == x] > 1]

-- 3.
posicionesPares :: [a] -> [a]
posicionesPares xs = [xs !! i | i <- [0 .. length xs - 2], even i]

-- 4.
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia xs ys = [x | x <- xs, length [y | y <- ys, x == y] == 0]

-- 5.
indicesPicos :: Ord a => [a] -> [Int]
indicesPicos xs = [i | i <- [1 .. length xs - 2],
    xs !! i > xs !! (i - 1),
    xs !! i > xs !! (i + 1)]
