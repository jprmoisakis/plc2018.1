fatSingle :: Int -> Int
fatSingle 0 = 1
fatSingle 1 = 1
fatSingle x = x * fatSingle (x - 1)

fat :: [Int] -> [Int]
fat [] = []
fat (x : xs) = (x * fatSingle (x - 1)) : fat xs

fatorial :: Int -> [Int]
fatorial 1 = [1]
fatorial x = [y | y <- fat [1..x]]