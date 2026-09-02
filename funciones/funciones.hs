{-
Definir las siguientes funciones en Haskell, incluyendo su signatura de tipo:
1. cincoInt: Dado un número entero, devuelve 5
2. cincoChar : Dado un caracter, devuelve 5
3. cincoBool : Dado un booleano, devuelve 5
4. odd ?: Devuelve True o False dependiendo si un número es impar o no.
5. even?: Devuelve True o False dependiendo si un número es par o no. Definirla usando odd?.
6. square: Dado un entero, devuelve su cuadrado.
7. absolute: Dado un entero, devuelve su valor absoluto.
8. sign: Dado un entero, devuelve:
1, si el entero es positivo
-1, si el entero es negativo
0, si el entero es 0
9. suc: Dado un entero, devuelve su sucesor.
10. predecesor : Dado un entero, devuelve su predecesor.
11. predecesorNat: Dado un entero, devuelve su predecesor. Si el entero es menor o igual que 0, devuelve 0.
12. idenInt: Dado cualquier número entero, lo devuelve.
13. idenChar : Dado cualquier caracter, lo devuelve.
14. idenBool : Dado cualquier booleano, lo devuelve.
15. double: Dado un entero, devuelve su doble.
16. roots: Dados los coeficientes de una ecuación de primer grado, devolver su raı́z (suponer que la ecuación
siempre tiene solución).
17. promedio: Dados 3 números, devolver su promedio.
18. xor : Dados dos booleanos, devolver el xor entre ellos.
19. modulo: Dado un par ordenado, devolver su módulo.
20. cuadrante: Dado un punto en el plano, devolver el cuadrante al que corresponde.
21. simetrico: Dado un punto en el plano, devolver su simétrico con respecto al origen.
22. dist: Dados dos puntos en el plano, devolver su distancia.
′23. roots : Dados los coeficientes de una ecuación de segundo grado, devolver sus raices (suponer que la
ecuación siempre tiene solución real).
24. app: Dada una función de enteros en enteros y un número entero, devuelve la función aplicada al elemento
25. appEither : Dadas dos funciones f y g de enteros en enteros y dos números enteros, aplique f al segundo
número si el primero es mayor o igual a 0, o g en el caso contrario.
-}

cincoInt :: Int -> Int
cincoInt x = 5

-- 2)
cincoChar :: Char -> Int
cincoChar x = 5

-- 3)
cincoBool :: Bool -> Int
cincoBool x = 5

-- 4)
odd' :: Int -> Bool
odd' x = (mod x 2) /= 0

-- 5)
even' :: Int -> Bool
even' x = not (odd' x)

-- 6)
square :: Int -> Int
square x = x * x

-- 7)
absolute :: Int -> Int
absolute x
    | x < 0 = (-x)
    | otherwise = x

-- 8)
sign :: Int -> Int
sign x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

-- 9)
suc :: Int -> Int
suc x = x + 1

-- 10)
predecesor :: Int -> Int
predecesor x = x - 1

-- 11)
predecesorNat :: Int -> Int
predecesorNat x
    | x <= 0 = 0
    | otherwise = predecesor x

-- 12)
idenInt :: Int -> Int
idenInt x = x

-- 13)
idenChar :: Char -> Char
idenChar x = x

-- 14)
idenBool :: Bool -> Bool
idenBool x = x

-- 15)
double :: Int -> Int
double x = x * 2

-- 16)
roots :: Int -> Int -> Int
roots a b = (-b) `div` a

-- 17)
promedio :: Float -> Float -> Float -> Float
promedio a b c = (a + b + c) / 3

-- 18)
xor :: Bool -> Bool -> Bool
xor a b = a /= b

-- 19)
modulo :: (Float, Float) -> Float
modulo (x, y) = sqrt (x ^ 2 + y ^ 2)

-- 20)
cuadrante :: (Float, Float) -> Int
cuadrante (x, y)
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | x > 0 && y < 0 = 4
    | otherwise = 0

-- 21)
simetrico :: (Float, Float) -> (Float, Float)
simetrico (x, y) = (-x, -y)

-- 22)
dist :: (Float, Float) -> Float
dist (x, y)
    | (x - y) > 0 = (x - y)
    | otherwise = -(x - y)

-- 23)
roots' :: (Float, Float, Float) -> (Float, Float)
roots' (a, b, c) = ((-b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a), (-b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a))

-- 24)
app :: (Int -> Int, Int) -> Int
app (f, x) = f x

-- 25)
appEither :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
appEither f g n1 n2
    | n1 >= 0 = f n2
    | otherwise = g n2
