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
distancia (Ponto a b c) (Ponto x y z) = sqrt( (a - x) ^ 2 + (b - y) ^ 2  + (c - z) ^ 2 )

-- Compreensão de lista pra pritar uma matriz
grid :: Int -> Int -> [(Int, Int)]
grid a b = [(x, y)| x <- aux a, y <- aux b] 

aux :: Int -> [Int]
aux n = [0..n]

-- Usar grid mas excluir a diagonal (n,n)
square :: Int -> [(Int, Int)]
square n = [(x, y)| x <- aux n, y <- aux n, x /= y]

-- Merge em duas listas e ord
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | y < x = y : merge (x : xs) ys

-- Merge Sort
halve :: Ord a => [a] -> ([a], [a])
halve xs = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (fst (halve xs))) (mergeSort (snd (halve xs)))

-- Aplicar lista de funcções unárias em lista de as
aplicaFuncoes :: [a -> a] -> [a] -> [[a]]
aplicaFuncoes _ [] = []
aplicaFuncoes [] b = []
aplicaFuncoes (f : fs) xs = map f xs : aplicaFuncoes fs xs

-- Dias da semana data, várias funções
data DiasSemana = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado
    deriving (Show, Enum, Eq, Ord)

ordenaUteis :: [DiasSemana] -> [DiasSemana]
ordenaUteis [] = []
ordenaUteis xs = mergeSort xs

datasIguais :: [(DiasSemana, Int)] -> DiasSemana -> [Int]
datasIguais [] _ = []
datasIguais (x : xs) dia 
    | fst x == dia = snd x : datasIguais xs dia
    | otherwise = datasIguais xs dia

imprimeMesAux :: Int -> DiasSemana -> [(Int, DiasSemana)]
--imprimeMesAux dia = [(a, b) | a <- [1..30], b <- [dia..] ++ [Domingo .. pred dia]]
imprimeMesAux 31 _ = []
imprimeMesAux a dia
    | dia /= Sabado = (a, dia) : imprimeMesAux (a + 1) (succ dia)
    | otherwise = (a, dia) : imprimeMesAux (a + 1) Domingo

imprimeMes :: DiasSemana -> [(Int, DiasSemana)]
imprimeMes dia = imprimeMesAux 0 dia