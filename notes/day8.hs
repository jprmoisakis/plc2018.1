adicao3 :: Int -> Int -> Int -> Int
adicao3 a b c = a + b + c

adicaoPar :: (Int, Int) -> Int
adicaoPar (x, y) = x + y

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

inc :: Int -> Int
inc x = x + 1

-- Funções da alta ordem sobre listas
-- Filtro -> [1, 2, 3, 4, 5] -> [2, 4]

filtro :: (t -> Bool) -> [t] -> [t]
filtro f [] = []
filtro f (x : xs)
    | f x = x : filtro f xs
    | otherwise = filtro f xs

ehPar :: Int -> Bool
ehPar x = x `mod` 2 == 0

ehImpar :: Int -> Bool
ehImpar x = x `mod` 2 /= 0

ehDigito :: Char -> Bool
ehDigito c = (c >= '0') && (c <= '9') -- isDigit import Data.Char

filtro2 :: (t -> Bool) -> [t] -> [t]
filtro2 f l = [ x | x <- l, f x]

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x : xs) = x + somaLista xs

prodLista :: [Int] -> Int
prodLista [] = 1
prodLista (x : xs) = x * prodLista xs

concatLista :: [[a]] -> [a]
concatLista [] = []
concatLista (x : xs) = x ++ concatLista xs

mfoldr :: (t -> t -> t) -> t -> [t] -> t
mfoldr f v [] = v 
mfoldr f v (x : xs) = f x (mfoldr f v xs)

mfoldr1 :: (t -> t -> t) -> [t] -> t
mfoldr1 f [v] = v 
mfoldr1 f (x : xs) = f x (mfoldr1 f xs)

mfoldl :: (t -> t -> t) -> t -> [t] -> t
mfoldl f v [] = v
mfoldl f v (x : xs) = mfoldl f (f v x) xs