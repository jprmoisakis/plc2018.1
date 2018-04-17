

-- Aplicacao de funcão com $
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- f ( g x ) = f $ g x

-- sum (filter (>10) (map (*2) [4..18]) )
-- sum $ filter (>10) (map (*2) [4..18])
-- sum $ filter (>10) $ map (*2) [4..18]
-- map ($ 3) [(4+) , (9*), (^3), sqrt]


twice :: (t -> t) -> t -> t
twice f x = (f . f) x

inc x = x + 1

iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = (iter (n-1) f)   . f

{-
(iter 3 inc) 5
=
((iter 2 inc ) . inc)
=
(((iter 1 inc)   .inc ) . inc)
=
((((iter 0 inc ). inc)   .inc ) . inc)
=
((( id . inc) . inc ) . inc)   
-}

iter 4 (/2) 2000
=
(iter 3 (/2)) . (/2)

addNum :: Int -> (Int -> Int)
addNum n = h
 where
    h m = n + m

-- Composicao de funcao
--{-
comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g = (\x y -> g (f x) (f y)) 
--}

multiplica :: Int -> Int -> Int
multiplica a b  = a * b 

dobraLista :: [Int] -> [Int]
dobraLista = map (multiplica 2)

{-
- Qual é o resultado da expressao
 zipWith ($) [sum, product] [[1,2], [3,4]]
-}
