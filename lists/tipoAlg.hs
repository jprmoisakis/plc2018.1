-- Fazer o MDC
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc 0 b = b
mdc a b 
    | a == b = a
    | (max a b) `mod` (min a b) == 0 = min a b
    | (max a b) `mod` (min a b) /= 0 = mdc (min a b) ((max a b) `mod` (min a b))

-- Saber se é primo
primos :: Int -> [Int]
primos n = crivo [2..n]

crivo :: [Int] -> [Int]
crivo [] = []
crivo (x : xs) = x : crivo[y | y <- xs, y `mod` x /= 0]

ehPrimo :: Int -> Bool
ehPrimo n = elem n (primos n)

-- distancia entre pontos
type Coord = Double
data Pontos = Ponto Coord Coord Coord
    deriving (Show)

distancia :: Pontos -> Pontos -> Double
distancia (Ponto a b c) (Ponto x y z) =
    sqrt( (a - x) ^ 2 + (b - y) ^ 2  + (c - z) ^ 2 )

-- Compreensão de lista pra pritar uma matriz
grid :: Int -> Int -> [(Int, Int)]
grid a b = [(x, y)| x <- aux a, y <- aux b] 

aux :: Int -> [Int]
aux n = [0..n]

-- Usar grid mas excluir a diagonal (n,n)
square :: Int -> [(Int, Int)]
square n = [(x, y)| x <- aux n, y <- aux n, x /= y]

-- Merge em duas listas e ord
merge :: [Int] -> [Int] -> [Int] -- Só funciona para duas listas já ordenadas
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | y <= x = y : merge (x : xs) ys

-- Merge Sort
halve :: [Int] -> ([Int], [Int])
halve [] = ([], [])
halve xs = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)

mergeSort :: [Int] -> [Int] -- Não funciona 100% por causa da limitação do merge
mergeSort [] = []
mergeSort xs = merge (fst (halve xs)) (snd (halve xs))

-- Aplicar lista de funcções unárias em lista de ints
aplicaFuncoes :: [Int -> Int] -> [Int] -> [[Int]]
aplicaFuncoes _ [] = []
aplicaFuncoes [] b = []
aplicaFuncoes (f : fs) xs = map f xs : aplicaFuncoes fs xs