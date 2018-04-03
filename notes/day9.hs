-- Composição de função

twice :: (t -> t) -> t -> t
twice f x = (f . f) x

inc x = x + 1

iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = (iter (n-1) f) . f

addNum :: Int -> (Int -> Int)
addNum n = h
    where
        h m = n + m

addNumLamb :: Int -> (Int -> Int)
addNumLamb n = (\m -> n + m)

comp2 :: (t -> u) -> (u -> u -> v) -> (t -> t -> v)
comp2 f g = (\x y -> g (f x) (f y))

multiplica :: Int -> Int -> Int
multiplica a b = a * b

dobraLista :: [Int] -> [Int]
dobraLista = map (multiplica 2)
