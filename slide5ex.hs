import Data.Char

vendas :: Int -> Int
vendas 0 = 3
vendas 1 = 4
vendas 2 = 10
vendas 3 = 6
vendas 4 = 7

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f x 
    | f x < f (x-1) = False
    | otherwise = isCrescent f (x-1)

-- Trying out map function
quadradoLista :: [Int] -> [Int]
quadradoLista [] = []
quadradoLista (x : xs) = x * x : quadradoLista xs

quadradoListaM :: [Int] -> [Int]
quadradoListaM xs = map (\ x -> x * x) xs

-- Trying out folding function
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x : xs) = x + somaLista xs

somaListaF :: [Int] -> Int
somaListaF xs = foldr (+) 0 xs

-- Trying out filter function
digits :: String -> String
digits xs = filter isDigit xs

-- Questão, elevar ao quadrado usando map
quadradoMap :: [Int] -> [Int]
quadradoMap xs = map (\ x -> x * x) xs

-- Questão, soma dos quadrados dos itens usando fold
somaQuadradoFold :: [Int] -> Int
somaQuadradoFold xs = foldr (\ x -> (+) (x * x)) 0 xs

-- Questão, retornar lista com os numeros maiores que zero usando filter
maiorQueZero :: [Int] -> [Int]
maiorQueZero xs = filter (> 0) xs

-- Questão, recebe lista de lista de inteiros e retorna lista com os maiores de cada sub lista
maiores :: [[Int]] -> [Int]
--maiores [] = []
--maiores (x : xs) = maximum x : maiores xs
maiores xs = map maximum xs