-- Vinícius Aguiar de Oliveira
-- 08/03/2018
-- Lista 1 PLC

import Data.Char

-- Questão 1
exclusiveOr :: Bool -> Bool -> Bool
exclusiveOr True y = not y -- DEALS WITH TRUE TRUE AND TRUE FALSE
exclusiveOr False y = y -- DEALS WITH FALSE TRUE AND FALSE FALSE

-- Questão 2

-- Questão 3
nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd _ _ = True

nAnd2 :: Bool -> Bool -> Bool
nAnd2 True True = False
nAnd2 True False = True
nAnd2 False _ = True

-- Questão 4
toUppercase:: Char -> Char
toUppercase a
    | fromEnum a >= 65 && fromEnum a <= 90 = a
    | otherwise = toEnum (fromEnum a - 32)

-- Questão 5
charToNum :: Char -> Int
charToNum a
    | isDigit a = digitToInt a

-- Exercicio 1 do Slide
addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos(n - 1)

-- Exercicio 2 do Slide
paraDireita :: Int -> String -> String
paraDireita n word = addEspacos n ++ word

-- Exercicio 3 do Slide
semana :: Int -> Int
semana 1 = 4
semana 2 = 0
semana 3 = 4
semana 4 = 2
semana 5 = 1
semana 6 = 0
semana 7 = 10
semana 8 = 9
semana 9 = 3
semana 10 = 5

imprimeCabecalho :: String
imprimeCabecalho = "Semana  Venda" 

-- Exercicio 4 do Slide
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (minThree a b c, maxThree a b c)
    where 
        minThree :: Int -> Int -> Int -> Int
        minThree a b c
            | a <= b && a <= c = a
            | b <= a && b <= c = b
            | otherwise = c
        maxThree :: Int -> Int -> Int -> Int
        maxThree a b c
            | a >= b && a >= c = a
            | b >= a && b >= c = b
            | otherwise = c

-- Exercicio 5 do Slide
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) = (minThree (a, b, c), midThree (a, b, c), maxThree (a, b, c))
    where
        minThree :: (Int, Int, Int) -> Int
        minThree (a, b, c)
            | a <= b && a <= c = a
            | b <= a && b <= c = b
            | otherwise = c
        maxThree :: (Int, Int, Int) -> Int
        maxThree (a, b, c)
            | a >= b && a >= c = a
            | b >= a && b >= c = b
            | otherwise = c
        midThree :: (Int, Int, Int) -> Int
        midThree (a, b, c)
            | a <= maxThree (a, b, c) && a >= minThree (a, b, c) = a
            | b <= maxThree (a, b, c) && b >= minThree (a, b, c) = b
            | otherwise = c

-- Exercicio 6 do Slide
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

pontoX :: Ponto -> Float
pontoX (x, y) = x

pontoY :: Ponto -> Float
pontoY (x, y) = y

vertical :: Reta -> Bool
vertical (a, b)
    | fst a == fst b = True
    | otherwise = False
