-- Retornar todas as sublistas de uma lista
-- sublista[1,2,3] = [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]]

sublistas :: [Int] -> [[Int]]
sublistas [] = [[]]
sublistas (x : xs) = [ x : ys| ys <- sublistas xs] ++ sublistas xs

-- 4a)
poli :: Integer -> Integer -> Integer -> (Integer -> Integer)
poli a b c = (\x -> (a*x*x) + (b*x) + c)

-- 4b)
listaPoli :: [(Integer, Integer, Integer)] -> [Integer -> Integer]
listaPoli l = [(\x -> (a*x*x) + (b*x) + c)| (a, b, c) <- l]

applyLista :: [Integer -> Integer] -> [Integer] -> [Integer]
applyLista _ [] = []
applyLista [] _ = []
applyLista (f : fs) (y : ys) = (f y) : applyLista fs ys

-- 4c)
appListaPoli :: [Integer -> Integer] -> [Integer] -> [Integer]
appListaPoli fs xs = [f x | (f, x) <- zip fs xs]

-- 5a)
data Mobile = Pendente Int | Barra Mobile Mobile

peso :: Mobile -> Int
peso (Pendente c) = c
peso (Barra a b) = peso a + peso b

-- 5b)
balanceado :: Mobile -> Bool
balanceado (Pendente _) = True
balanceado (Barra a b) = balanceado a && balanceado b && peso a == peso b

-- 3)
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = (f x) : altMap g f xs