-- Fazer o MDC
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc 0 b = b
mdc a b 
    | a == b = a
    | (max a b) `mod` (min a b) == 0 = min a b
    | (max a b) `mod` (min a b) /= 0 = mdc (min a b) ((max a b) `mod` (min a b))

-- Saber se é primo

-- distancia entre pontos
type Coord = Double
data Pontos = Ponto Coord Coord Coord
    deriving (Show)

distancia :: Pontos -> Pontos -> Double
distancia (Ponto a b c) (Ponto x y z) =
    sqrt( (a - x) ^ 2 + (b - y) ^ 2  + (c - z) ^ 2 )

-- Compreensão de lista pra pritar uma matriz
grid :: Int -> Int -> [(Int, Int)]
grid 0 0 = [(0, 0)]

