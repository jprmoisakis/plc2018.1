import Data.Char

-- Questao 1
double :: [Int] -> [Int]
double [] = []
double (x : xs) = x * 2 : double xs

member :: [Int] -> Int -> Bool
member [] _ = False
member (x : xs) num
    | x == num = True
    | otherwise = member xs num

digits :: String -> String
digits [] = []
digits (x : xs) 
    | isDigit x = x : digits xs
    | otherwise = digits xs

sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs ((a, b) : xs) = a + b : sumPairs xs

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros ((a, b) : xs) p
    | a == p = b : livros xs p
    | otherwise = livros xs p

baseExemplo :: BancoDados
baseExemplo = [("Joao","Software Abstractions"), ("Andre","Programming in Haskell"), 
               ("Fernando","Introduction to Programming with Python"), ("Fernando","Programming in Haskell")]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] _ = []
emprestimos ((a, b) : xs) l 
    | b == l = a : emprestimos xs l
    | otherwise = emprestimos xs l

emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado ((a, b) : xs) l
    | b == l = True
    | otherwise = False

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos ((a, b) : xs) p
    | a == p = 1 + qtdEmprestimos xs p
    | otherwise = qtdEmprestimos xs p

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] _ _ = []
emprestar x p l = x ++ [(p, l)]

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] _ _ = []
devolver ((a, b) : xs) p l 
    | a == p && b == l = devolver xs p l
    | otherwise = (a, b) : devolver xs p l

-- Repetir questões com compreensão de lista
membro :: [Int] -> Int -> Bool
membro xs num = head [True | x <- xs, x == num]

livrosComp :: BancoDados -> Pessoa -> [Livro]
livrosComp xs p = [snd l | l <- xs, fst l == p]

emprestimosComp :: BancoDados -> Livro -> [Pessoa]
emprestimosComp xs l = [fst p | p <- xs, snd p == l]

emprestadoComp :: BancoDados -> Livro -> Bool 
emprestadoComp xs l = length [True | x <- xs, snd x == l] > 0
-- This only works when there is the book in the list
-- I could make it return a list that would be empty when
-- the book is not on the list, but that would return a list
-- instead of a Bool.

qtdEmprestimosComp :: BancoDados -> Pessoa -> Int
qtdEmprestimosComp xs p = sum [1 | x <- xs, fst x == p]

devolverComp :: BancoDados -> Pessoa -> Livro -> BancoDados
devolverComp xs p l = [x | x <- xs, fst x /= p, snd x /= l]