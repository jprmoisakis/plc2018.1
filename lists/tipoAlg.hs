-- Fazer o MDC
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc 0 b = b
mdc a b 
    | a == b = a
    | (max a b) `mod` (min a b) == 0 = min a b
    | (max a b) `mod` (min a b) /= 0 = mdc (min a b) ((max a b) `mod` (min a b))

