import Data.Char

double :: [Int] -> [Int]
double [] = []
double (x:xs) = x*2 : double xs

member :: [Int] -> Int -> Bool
member [] _ = False
member (x:xs) a
    | x == a = True
    | otherwise = member xs a

digits :: String  -> String
digits [] = []
digits (x:xs)
    | isDigit x = x : digits xs
    | otherwise = digits xs


sumPairs :: [(Int,Int)] -> [Int]
sumPairs [] = []
sumPairs ((a,b):xs) = (a + b) : sumPairs xs

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Joao","Software Abstractions"), ("Andre","Programming in Haskell"), ("Fernando","Introduction to Programming with Python"), ("Fernando","Programming in Haskell")]

livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros ((a,b):xs) p
        | a == p =  b : livros xs p
        | otherwise = livros xs p


emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] _ = []
emprestimos ((a,b):xs) l
        | b == l = a : emprestimos xs l
        | otherwise = emprestimos xs l


emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado ((a,b):xs) l
        | b == l = True
        |otherwise = emprestado xs l

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos ((a,b):xs) p
        | a == p =  1 + qtdEmprestimos xs p
        | otherwise = qtdEmprestimos xs p

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] _ _ = []
emprestar x p l = x ++ [(p,l)]

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] _ _ = []
devolver ((a,b):xs) p l
        | p == a && b == l = devolver xs p l
        | otherwise =  (a,b) : devolver xs p l

membro :: [Int] -> Int -> Bool
membro a b = head [True | k <- a, k== b]



--livros :: BancoDados -> Pessoa -> [Livro]
--emprestimos :: BancoDados -> Livro -> [Pessoa]
--emprestado :: BancoDados -> Livro -> Bool
--qtdEmprestimos :: BancoDados -> Pessoa -> Int
--devolver :: BancoDados -> Pessoa -> Livro -> BancoDados      
