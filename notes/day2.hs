-- maxi is in day1.hs using guard
maxi2 :: Int -> Int -> Int
maxi2 x y =
    if x >= y
    then x
    else y

ex0r :: Bool -> Bool -> Bool
ex0r x y = (x || y) && not (x && y) -- Ou Exclusivo (XOR)

ex0rCasamPadrao :: Bool -> Bool -> Bool
ex0rCasamPadrao True x = not x
ex0rCasamPadrao False x = x

mAnd :: Bool -> Bool -> Bool -- AND
mAnd True True = True
mAnd True False = False
mAnd False True = False
mAnd False False = False

mAnd2 :: Bool -> Bool -> Bool -- AND
mAnd2 True True = True
mAnd2 True False = False
mAnd2 False _ = False

mAnd3 :: Bool -> Bool -> Bool -- AND
mAnd3 True b = b
mAnd3 _ _ = False

-- RECURSÂO
fatorial :: Int -> Int
fatorial 1 = 1 -- CASO BASE
fatorial n = n * fatorial (n - 1) -- CASO RECURSIVO

vendas :: Int -> Int
vendas 0 = 3
vendas 1 = 4
vendas 2 = 20
vendas 3 = 0
vendas 4 = 9
vendas 5 = 30 

totalVendas :: Int -> Int
totalVendas n
    | n == 0 = vendas 0
    | otherwise = vendas n + totalVendas (n - 1)

maxi :: Int -> Int -> Int
maxi x y
    | x >= y = x
    | otherwise = y

maiorVenda :: Int -> Int
maiorVenda n
    | n == 0 = vendas 0
    | otherwise = maxi (vendas n) (maiorVenda (n - 1))

totalVendas2 :: Int -> Int
totalVendas2 0 = vendas 0
totalVendas2 n = vendas n + totalVendas2 (n - 1)

vendasZero :: Int -> Bool
vendasZero n =
    if vendas n == 0
    then True
    else vendasZero (n - 1)

vendasZero2 :: Int -> Bool
vendasZero2 n
    | vendas n == 0 = True
    | n == 0 = False
    | otherwise = vendasZero2 (n - 1)
