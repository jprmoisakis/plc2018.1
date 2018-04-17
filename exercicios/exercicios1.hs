-- DEFINA FATORIAL
fat :: Int -> Int -- RECEBE INT E RETURN INT
fat 1 = 1 -- CASO BASE
fat n = fat (n -1) * n -- CASO RECURSIVO

-- COMPARA 4
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d
    | a == b && a == c && a == d = True
    | otherwise = False

equalCount :: Int -> Int -> Int -> Int
equalCount a b c 
    | a == b && a /= c = 2
    | a == b && a == c = 3
    | a == c && a /= b = 2
    | b == c && a /= b = 2
    | otherwise = 0

-- DEFINIÇÕES LOCAIS
sumSquares :: Int -> Int -> Int
sumSquares a b = squareX + squareY
    where squareX = a * a
          squareY = b * b

sumSquares2 :: Int -> Int -> Int
sumSquares2 a b = square a + square b
    where square z = z * z

sumSquares3 :: Int -> Int -> Int
sumSquares3 a b = let squareX = a * a
                      squareY = b * b
                  in  squareX + squareY

maxiThree :: Int -> Int -> Int -> Int
maxiThree a b c 
    | a >= b && a >= c = a
    | b >= a && b >= c = b
    | otherwise = c

equalCount2 :: Int -> Int -> Int -> Int -> Int
equalCount2 a b c d
    | a == b && a == c && a == d = 3
    | a == b && a == c && a /= d = 2
    | a == b && a /= c && a == d = 2
    | a /= b && a == c && a == d = 2
    | otherwise = 1

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs m n p = (mx, eqCount)
    where mx = maxiThree m n p
          eqCount = equalCount2 mx m n p

semana :: Int -> Int
semana 1 = 4
semana 2 = 0
semana 3 = 4
semana 4 = 2
semana 5 = 1
semana 6 = 0
semana 7 = 10
semana 8 = 9
semana 9 = 3
semana 10 = 5

semanaVenda :: Int -> Int -> Int
semanaVenda _ 0 = 0 -- Caso Base
semanaVenda s n = if s /= semana n
                  then semanaVenda s (n-1)
                  else semanaVenda s (n-1) + 1
