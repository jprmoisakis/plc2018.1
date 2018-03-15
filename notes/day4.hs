-- Listas
-- Função ":" unir elemento ao início da lista
-- Funçao ".." lista o intervano na lista
-- drop para remover quantidade certa de elementos de lista, esquerda para direita
-- last para pegar ultimo elemento, take é como drop mas salva os elementos esquerda direita
-- init dropa o ultimo elemento apenas, head pega o primeira, tail o último
-- função "!!" tira o valor de certo lugar [1..5] !! 2 = 3
-- função "++" concatenar lista 
-- função "reverse" retorna a lista inversa
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

tamLista :: [a] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

double :: [Int] -> [Int]
double [] = []
double (x : xs) = (x * 2) : double xs

member :: [Int] -> Int -> Bool
member [] x = False
member (x : xs) y 
    | x == y = True
    | otherwise = member xs y

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

repeticao :: Int -> Char -> [Char]
repeticao 0 _ = []
repeticao n c = c : repeticao (n-1) c

mtake :: Int -> [a] -> [a]
mtake 0 _ = []
mtake n (x : xs) = x : mtake (n-1) xs
