import Data.Char

addMe :: Integer -> Integer -> Integer
addMe x y = x + y

square :: Int -> Int
square x = x * x

answer :: Int
answer = 42

greater :: Bool
greater = answer > 10

allEqual :: Int -> Int -> Int -> Bool
allEqual n p m = (n==m) && (m ==p)

maxi :: Int -> Int -> Int
maxi n m | n >= m = n
         | otherwise = m

vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 1
vendas 2 = 2
vendas 3 = 3
vendas 4 = 4 
vendas 5 = 5


totalVendas :: Int -> Int
totalVendas n | n == 0 = vendas 0
              | otherwise = vendas(n) + totalVendas(n-1)

fib  :: Int -> Int 
fib n | n == 1  = 1
      | n == 0 = 0
      | otherwise = fib(n-1) + fib(n-2)


intP :: (Int,Int)
intP = (33,43)

shift :: ((Int,Int), Int) -> (Int, (Int, Int))
shift ((x, y), z) = (x, (y, z))

umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = -b/(2.0 * a)

duasRaizes :: Float -> Float -> Float -> (Float, Float)
duasRaizes a b c =  (d - e, d+e)
      where d = -b/(2.0 * a)
            e = sqrt(b^2 - 4*a*c)/2*a
        

segGrau :: (Float, Float, Float) -> String
segGrau (a, b, c) 
    | b^2 == 4*a*c = show(umaRaiz a b c)
    | b^2 > 4*a*c = show f ++ " " ++ show s
    | otherwise = "Sem raizes"
      where(f,s) = duasRaizes a b c

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (menor, maior)
    where
        menor
            | a <= b && a <= c = a
            | b <= a && b <= c = b
            | otherwise = c
        
        maior
            | a >= b && a >= c = a
            | b >= a && b >= c = b
            | otherwise = c

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a,b,c) = (primeiro, meio, ultimo) 
    where
      primeiro
            | a <= b && a <= c = a
            | b <= a && b <= c = b
            | otherwise = c

      ultimo 
            | a >= b && a >= c = a
            | b >= a && b >= c = b
            | otherwise = c

      meio
            | ((primeiro == a) && (ultimo == b)) || ((primeiro == b) && (ultimo == a)) = c 
            | ((primeiro == c) && (ultimo == a)) || ((primeiro == a) && (ultimo == c)) = b
            | otherwise = a


type Ponto = (Float,Float)
type Reta = (Ponto, Ponto)

primeiraCordenadaPonto :: Ponto -> Float
primeiraCordenadaPonto (x,y) = x

segundaCordenadaPonto :: Ponto -> Float
segundaCordenadaPonto (x,y) = y

retaVertical :: Reta -> String
retaVertical ((x,y),(z,k))
           | (x == z) = show "eh vertical"
           | otherwise = show "nao eh vertical" 


ouExclusivo :: Bool -> Bool -> Bool
ouExclusivo True False = True
ouExclusivo False True = True
ouExclusivo _ _ = False          

nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd _ _ = True

nAnd2 :: Bool -> Bool -> Bool
nAnd2 x y
      | (x && y) == True = False
      | otherwise = True


offset = fromEnum 'A' - fromEnum 'a'

maiuscula :: Char -> Char
maiuscula ch 
          |(ch >= 'a' && ch < 'z') = toEnum(fromEnum ch + offset)
          | otherwise = ch 

palavraMaiuscula :: String -> String
palavraMaiuscula [] = []
palavraMaiuscula (x:xs) = maiuscula x : palavraMaiuscula(xs)

charToNum :: Char -> Int
charToNum x = fromEnum x - 48

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos x = " " ++ addEspacos(x-1)

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList(xs)

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = x*2 : doubleList xs

membership :: [Int] -> Int -> Bool
membership [] _ = False 
membership (x:xs) k 
         | x == k = True
         | otherwise = membership xs k 

sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs ((a, b):xs) = a + b : sumPairs xs

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo =
 [("Sergio","O Senhor dos Aneis"),
 ("Andre","Duna"),
 ("Fernando","Jonathan Strange & Mr.Norrell"),
 ("Fernando","Duna")] 

livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros ((a,b) : xs) p
          | a == p = b : livros xs p
          | otherwise = livros xs p

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] _ = []
emprestimos ((a,b) : xs) l 
          | b == l = a : emprestimos xs l
          | otherwise = emprestimos xs l

emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado ((a,b) : xs) l
          | b == l  = True
          | otherwise = emprestado xs l

sumPairss :: [(Int,Int)] -> [Int]
sumPairss lp = [a+b|(a,b) <- lp]

livross :: BancoDados -> Pessoa -> [Livro]
livross bd p = [b | (a,b) <- bd, p == a]

emprestimoss :: BancoDados -> Livro ->[Pessoa] 
emprestimoss bd l = [ a |(a,b) <- bd, l == b]

emprestadoo :: BancoDados -> Livro -> [Bool]
emprestadoo bd l = [True | (a,b) <- bd, l == b]

qtdEmprestimoss :: BancoDados -> Pessoa -> Int 
qtdEmprestimoss bd p = [] 
