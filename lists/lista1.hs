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

