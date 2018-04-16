-- Menor inteiro de uma lista
menor :: [Int] -> Int
menor xs = minimum xs

-- myMap
-- Map é para que a função rode em cada elemento da lista, nesse caso temos duas listas para suprir
-- a função que tem 2 parametros, cada parametro sera um elemento de cada lista
-- as listas precisam ter tamanhos iguais
-- Uma vez que rodamos a função na head das listas "concatenamos" (:) com o myMap de f nas caudas
myMap :: (a -> b -> b) -> [a] -> [b] -> [b]
myMap f [] [] = []
myMap f (x : xs) (y : ys) = f x y : myMap f xs ys

-- Fibonacci
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Sublistas de uma lista
sublista :: [Int] -> [[Int]]
sublista [] = [[]]
sublista (x : xs) = [x : ys| ys <- sublista xs] ++ sublista xs

-- Quicksort
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x : xs) = quicksort[y| y <- xs, y < x] ++ [x] ++ quicksort[y| y <- xs, y >= x]