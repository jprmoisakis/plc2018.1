

-- Listas

-- type String = [Char]

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

tamLista :: [a] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

double :: [Int] -> [Int]
double [] = []
double (x:xs) = (2 * x)  : double xs

member :: [Int] -> Int -> Bool
member [] _    = False
member (x:xs) y  
  | x == y    = True
  | otherwise = member xs y 

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

repeticao :: Int -> Char -> [Char]
repeticao 0 c  = []
repeticao n c  = c : repeticao (n-1) c   

mtake :: Int -> [a] -> [a]
mtake n []     =  []
mtake 0 _      =  []
mtake n (x:xs) = x : mtake (n-1) xs 