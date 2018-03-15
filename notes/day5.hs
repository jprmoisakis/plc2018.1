mdrop :: Int -> [a] -> [a]
mdrop 0 l = l
mdrop _ [] = []
mdrop n (x:xs) = [] ++ mdrop (n - 1) xs

-- Ordenacao por insercao

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = inserir x (iSort xs)

inserir :: Int -> [Int] -> [Int]
inserir n [] = [n]
inserir n (x : xs)
    | n <= x = n:x:xs
    | otherwise = x : inserir n xs

-- Exercicios

membroLista :: Int -> [Int] -> Bool
membroLista n [] = False
membroLista n (x : xs)
    | n == x = True
    | otherwise = membroLista n xs

-- digitosLista "osjg1lk3kj2" retornoa "132"
digitosLista :: [Char] -> [Char]
digitosLista [] = []
digitosLista (x : xs)
    | ehDigito x = x : digitosLista xs
    | otherwise = digitosLista xs
ehDigito :: Char -> Bool
ehDigito ch = ('0' <= ch) && (ch <= '9')

--somarParesLista :: [(Int, Int)] -> [Int]
somarParesLista :: [(Int, Int)] -> [Int]
somarParesLista [] = []
somarParesLista (x : xs) = (fst x + snd x) : somarParesLista xs

-- Expressao case

head1 :: [a] -> a
head1 [] = error "Lista Vazia"
head1 (x : _) = x

head2 :: [a] -> a
head2 xs = case xs of
    [] -> error "Lista vazia"
    (x : _) -> x

-- Compreensao de lista

dobrarLista :: [Int] -> [Int]
dobrarLista [] = []
dpbrarLista (x : xs) = (2*x) : dobrarLista xs

dobrarListaComp :: [Int] -> [Int]
dobrarListaComp xs = case xs of
    [] -> error "Lista Vazia"
    xs -> [2 * x | x <- xs]

ehPar :: Int -> Bool
ehPar x = x `mod` 2 == 0

dobrarValoresPares :: [Int] -> [Int]
dobrarValoresPares [] = []
dobrarValoresPares (x : xs)
    | ehPar x = (2 * x) : dobrarValoresPares xs
    | otherwise = dobrarValoresPares xs

dobrarValoresParesComp :: [Int] -> [Int]
dobrarValoresParesComp xs = case xs of
    [] -> error "Lista Vazia"
    xs -> [2 * x | x <- xs, ehPar x] -- Depois da virgula é a condição

somaPares :: [(Int, Int)] -> [Int]
somaPares xs = case xs of
    [] -> error "Lista vazia"
    xs -> [x + y | (x, y) <- xs]

