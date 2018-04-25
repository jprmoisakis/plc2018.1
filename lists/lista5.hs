-- 1a)
f :: [Int] -> [Int]
f (_ : []) = []
f (a : b : xs)
    | a == b = b : f (b : xs)
    | otherwise = f (b : xs)

-- 1b)
f2 :: [Int] -> [Int]
f2 (x : xs) = [y | y <- xs, y == x] ++ f2 xs

-- 2)
g :: [Int] -> Bool
g [] = True
g (x : xs) 
    | ((x > 10 || x <= 100) && x `mod` 2 == 0) || (x <= 10 || x > 100) = True && g xs
    | otherwise = False

-- 2) Using map, filter, foldr
g2 :: [Int] -> Bool
g2 xs = 
    if (foldr (+) 0 (map (`mod` 2) (filter (<= 100) (filter (> 10) xs)))) > 0
    then False
    else True

-- 3a)
type Nome = String
type Potencia = Float
data Lampada = Compacta Nome Potencia | Incandescente Nome Potencia

-- 3b)
instance Show Lampada where
    show (Compacta n p) = "Compacta " ++ show n ++ " " ++ show p
    show (Incandescente n p) = "Incandescente" ++ show n ++ " " ++ show p

-- 3c)
instance Eq Lampada where
    (Compacta n1 p1) == (Compacta n2 p2) = n1 == n2 && p1 == p2
    (Incandescente n1 p1) == (Incandescente n2 p2) = n1 == n2 && p1 == p2

-- 4a)
data Barra = Um Fio Lampada | Dois Fio Barra
data Lustre = End Pendente Lampada | Beg [Barra]
    