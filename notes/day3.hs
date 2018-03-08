offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

maiuscula :: Char -> Char
maiuscula ch = toEnum (fromEnum ch + offset)

ehDigito :: Char -> Bool
ehDigito ch = ('0' <= ch) && (ch <= '9')

primeiro :: (Int, Int) -> Int
primeiro (x, y) = x
--primeiro (x, _) = x

segundo :: (Int, Int) -> Int
segundo (x, y) = y

primeiroTripla :: (Int, Int, Int) -> Int
primeiroTripla (x, y, z) = x

shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((x, y), z) = (x, (y, z))

extraiValor :: (Bool, (Int, Bool), Char) -> Char
extraiValor (_, (_, _), a) = a -- or extraiValor (_, _, a) = a

-- Sinonimos de tipos

type ParInteiro = (Int, Int)

primeiroParInteiro :: ParInteiro -> Int
primeiroParInteiro (x, _) = x

type Nome = String
type Idade = Int
type Telefone = Int
type Pessoa = (Nome, Idade, Telefone)

nome :: Pessoa -> Nome
nome (n, idd, tlf) = n

-- Equação do segundo grau

umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = -b / (2 * a)

duasRaizes :: Float -> Float -> Float -> (Float, Float)
duasRaizes a b c = (d - e, d + e)
    where
        d = -b / (2 * a)
        e = sqrt (b ^ 2 - (4.0 * a * c)) / (2.0 *a)

raizes :: Float -> Float -> Float -> String
raizes a b c
    | b ^ 2 == 4.0 * a * c = show (umaRaiz a b c)
    | b ^ 2 > 4.0 * a * c = show f ++ " " ++ show s
    | otherwise = show "Nao ha raizes"
        where 
            (f, s) = duasRaizes a b c
            -- f = fst (duasRaizes a b c)
            -- s = snd (duasRaizes a b c)
