answer :: Int
answer = 42

greater :: Bool
greater = (answer > 71)

yes :: Bool
yes = True

square :: Int -> Int
square x = x * x

allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = (a == b) && (b == c)

maxi :: Int -> Int -> Int
maxi a b
	| a >= b	= a
	| otherwise	= b

addD :: Int -> Int -> Int
addD a b = 2 * (a + b)

-- RECURSÃO COMEÇA AQUI

vendas :: Int -> Int
vendas a = a
totalVendas :: Int -> Int
totalVendas n
	| n == 0	= vendas 0
	| otherwise	= totalVendas (n-1) + vendas n
