
mdrop :: Int -> [a] -> [a]
mdrop 0 lista  = lista 
mdrop _ []     = []
mdrop n (x:xs) = [] ++ mdrop (n-1) xs 

-- Ordenacao por insercao

iSort :: [Int] -> [Int]
iSort []     = []
iSort (x:xs) =  inserir x (iSort xs)

inserir :: Int -> [Int] -> [Int]
inserir n []     = [n]
inserir n (x:xs)
  | n <= x       = n:(x:xs)
  | otherwise    = x: inserir n xs

{-
iSort [5,3,2]
= {aplicacao de iSort}
inserir 5 (iSort [3,2])
= {aplicacao de iSort}
inserir 5 (inserir 3 (iSort [2]))
= {aplicacao de iSort}
inserir 5 ((inserir 3 (inserir 2: iSort []))
= {aplicacao de iSort}
inserir 5 ((inserir 3 (inserir 2:[]))
= {aplicacao de inserir}
inserir 5 (inserir 3 [2])
= {aplicacao de inserir}
inserir 5 (2:inserir 3 [] ))
= {aplicacao de inserir}
inserir 5 (2:[3])
= {aplicacao de inserir}
inserir 5 [2,3]
= {aplicacao de inserir}
2: inserir 5 [3]
= {aplicacao de inserir}
2: (3: inserir 5 []) 
= {aplicacao de inserir}
2: (3:[5])
= {aplicacao de (:)}
[2,3,5]    
-}

--Exercicios

--membroLista :: Int -> [Int] -> Bool
-- membroLista 3 [1,2,3] = True
-- membroLista 5 [1,2,3] = False

--digitosLista :: [Char] -> [Char]
-- digitosLista "1sdr3t7u" = "137"

--somarParesLista :: [(Int,Int)] -> [Int]
--somarParesLista [(1,2), (4,5)] = [3,9]

-- Expressao case

head1 :: [a] -> a
head1 [] = error "Lista vazia"
head1 (x:_) = x

head2 :: [a] -> a
head2 lista = case lista of
              [] -> error "Lista vazia"
              (x:_) -> x

-- Compreensao de lista

dobrarLista :: [Int] -> [Int]
dobrarLista [] = []
dobrarLista (x:xs) = (2*x) : dobrarLista xs

dobrarListaCompreensao :: [Int] -> [Int]
dobrarListaCompreensao l = [(*) 2 x| x <- l ]

ehPar :: Int -> Bool
ehPar x = x `mod` 2 == 0

dobrarValoresPares :: [Int] -> [Int]
dobrarValoresPares lista = [ 2 * x | x <- lista, ehPar x, x > 2  ]

somaPares :: [(Int,Int)] -> [Int]
somaPares lista = [ x + y | (x,y) <- lista, x > y ]

