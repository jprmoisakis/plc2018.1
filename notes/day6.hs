-- Expressao case permite casamento de padrao
-- Não apenas argumentos de funções

import Data.Char

ehDigito :: Char -> Bool
ehDigito ch = (ch >= '0') && (ch <= '9') -- isDigit x mesma coisa

ehChar :: Char -> Bool
ehChar ch = (ch >= 'a') && (ch <= 'Z')

ehPar :: Int -> Bool
ehPar ch 
    | (ch `mod` 2) == 0 = True
    | otherwise = False

digits :: String -> String
digits [] = []
digits (x : xs)
    | ehDigito x = x : digits xs
    | otherwise = digits xs

firstDigit :: String -> Char
firstDigit st = case digits st of 
    [] -> '\0'
    (a : as) -> a

-- Compreensão de lista

somaPares :: [(Int, Int)] -> [Int]
somaPares l = [x + y | (x, y) <- l]

tamLista :: [a] -> Int
tamLista l = sum [1 | _ <- l]

triangulos = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10]]

trianguloRetangulo = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a ^ 2 + b ^ 2 == c ^ 2]