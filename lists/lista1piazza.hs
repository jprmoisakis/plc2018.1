-- https://piazza-resources.s3.amazonaws.com/j652bus99z66yt/j6nkf54g3dj423/sala_lista1.pdf?AWSAccessKeyId=AKIAIEDNRLJ4AZKBW6HA&Expires=1521397902&Signature=WSMATOn%2BHV2DeAGBYl%2FfNi%2BpfW0%3D
-- Lista é a do link acima
-- Vinícius Aguiar de Oliveira

import Data.Char

-- Questão 1
quantosIguais :: Eq a => a -> a -> a -> Int -- Não entendi esse (Eq a)
quantosIguais x y z 
    | x == y && x == z = 3
    | x == y && x /= z = 2 -- x y
    | x == z && x /= y = 2 -- x z
    | y == z && x /= y = 2 -- y z
    | otherwise = 0

-- Questão 2
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (menor a b c, maior a b c)
    where
        menor :: Int -> Int -> Int -> Int
        menor a b c
            | a <= b && a <= c = a
            | b <= a && b <= c = b
            | otherwise = c
        maior :: Int -> Int -> Int -> Int
        maior a b c
            | a >= b && a >= c = a
            | b >= a && b >= c = b
            | otherwise = c

-- Questão 3 Qual é um jeito melhor de fazer isso?
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) = (menorTripla (a, b, c), meioTripla (a, b, c), maiorTripla (a, b, c))
        where
            menorTripla :: (Int, Int, Int) -> Int
            menorTripla (a, b, c)
                | a <= b && a <= c = a
                | b <= a && b <= c = b
                | otherwise = c
            maiorTripla :: (Int, Int, Int) -> Int
            maiorTripla (a, b, c)
                | a >= b && a >= c = a
                | b >= a && b >= c = b
                | otherwise = c
            meioTripla :: (Int, Int, Int) -> Int -- COMO FAZER ISSO
            meioTripla (a, b, c)
                | a <= maiorTripla (a, b, c) && a >= menorTripla (a, b, c) = a
                | b <= maiorTripla (a, b, c) && b >= menorTripla (a, b, c) = b
                | otherwise = c

-- Questão sobre listas
double :: [Int] -> [Int]
double [] = []
double (x : xs) = (x * 2) : double xs

-- Questao 2
member :: [Int] -> Int -> Bool
member [] _ = False
member (x : xs) num
    | x == num = True
    | otherwise = member xs num

-- Questao 3
digits :: String -> String
digits [] = []
digits (x : xs)
    | isDigit x = x : digits xs
    | otherwise = digits xs

-- Questao 4 só funciona com listas de tamanho iguais, tá certo?
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] [] = []
sumPairs (x : xs) (y : ys) = x + y : sumPairs xs ys

