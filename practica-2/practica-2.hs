{-
1. head ′: Dada una lista, devuelve su primer elemento
2. tail ′ : Dada una lista, devuelve la cola de la lista
3. last : Dada una lista, devuelve su último elemento
4. cons ′: Dada una lista y un elemento, concatena el elemento al inicio de la lista
5. snoc ′ : Dada una lista y un elemento, concatena el elemento al final de la lista
6. conssnoc : Dada una lista y un elemento, concatena el elemento al inicio y al final de la lista
7. take ′: Dado un número entero n y una lista, devuelve los primeros n elementos de la lista
8. drop ′ : Dado un número entero n y una lista, saca los primeros n elementos de la lista
9. replicate : Dado un número entero n y un elemento a, devuelve una lista de n elementos donde todos son a
10. reverse : Dada una lista, la invierte
	a) Definir usando la función ++ del preludio que dadas dos listas las concatena. 
		Por ejemplo: [1,2,3] ++ [4,5] = [1,2,3,4,5]
	b) Definir sin usar ++
11. Ayuda: Definir su propio ++
12. insertNth ′: Dado un número entero n, un elemento x y una lista, devuelve una lista similar a la original, pero
	donde se inserta el elemento x en la posicion n
13. sum ′: Dada una lista de enteros, devuelve la suma de todos los elementos de la lista
14. prod ′ : Dada una lista de enteros, devuelve el producto de todos los elementos de la lista
15. filterOdd ′: Dada una lista de enteros, devuelve una lista donde los únicos elementos son los números impares de 
	la lista de entrada
16. filterEven ′ : Dada una lista de enteros, devuelve una lista donde los únicos elementos son los números pares de
	la lista de entrada
17. zip ′ : Dadas dos listas, devuelve una lista donde cada elemento es un par de elementos formado por los elementos
	de cada lista
18. singleton ′: Dado un elemento, devuelve una lista donde el único elementos es el dado
19. or ′: Dado una lista de valores Booleanos devuelve True si algún elemento de la lista es True, y False en caso
	contrario
20. and ′: Dado una lista de valores Booleanos devuelve True si todos los elemento de la lista son True, y False en
	caso contrario
21. longitudes: Dada una lista de listas devuelve una lista de las longitudes de cada lista.
	Por ejemplo: longitudes [[1,2,3], [4,5], []] = [3,2,0]
	a) Definir usando la función length del preludio que dada una lista devuelve la cantidad de elementos que tiene
	b) Definir sin usar length
22. masDe: Dada una lista de listas xss y un número n, devuelve la lista de aquellas listas de xss con longitud mayor
	que n
-}

-- 1)
head' :: [a] -> a
head' (x:_) = x

-- 2)
tail' :: [a] -> [a]
tail' (_:xs) = xs

-- 3)
last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs

-- 4)
cons' :: [a] -> a -> [a]
cons' x xs = xs:x

-- 5)
snoc' :: [a] -> a -> [a]
snoc' [] x = [x]
snoc' (x:xs) y = x:snoc' xs y
