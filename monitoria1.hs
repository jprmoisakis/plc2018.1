-- Menor inteiro de uma lista
menor :: [Int] -> Int
menor xs = minimum xs

-- myMap
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