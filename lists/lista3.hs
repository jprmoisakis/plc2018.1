sort :: Eq a => (a -> a -> Bool) -> [a] -> [a]
sort f [] = []
sort f (x : xs) = sort f [y| y <- xs, f y x] ++ [x] ++ sort f [y| y <- xs, (==) x y] ++ sort f [y| y <- xs, f x y]

agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar (x : xs) = (([y | y <- xs, y == x] ++ [x]) : []) ++ agrupar [y | y <- xs, x /= y]

type Lado = Float
type Triangulo = (Lado, Lado, Lado)

area :: [Triangulo] -> Float
area [] = 0
area xs = foldr1 (+) (map (\ (a, b, c) -> (a * b) / 2) xs)

functions :: [(a -> a -> a)] -> [a] -> [(a -> a)]
functions [] _ = []
functions (f : fs) (x : xs) = (f x) : functions fs xs

applyFunctions :: [(a -> a)] -> [a] -> [a]
applyFunctions [] _ = []
applyFunctions (f : fs) (x : xs) = (f x) : applyFunctions fs xs

abrev :: [String] -> [String]
abrev [] = []
abrev (x : xs) = sort (<) ((take 1 ((take 1 (words x)) !! 0) ++ ". " ++ ((drop 2 (words x)) !! 0)) : abrev xs)

agrupar2 :: Eq a => [a] -> [[a]]
agrupar2 (x:xs) = (x : [ y | y <- xs, x == y]) : []