{- primos :: [Int]
primos = crivo [2..]

crivo :: [Int] -> [Int]
crivo (p:xs) = p : crivo [x | x <- xs, x `mod` p /= 0]

ehPrimo n = elem n (takeWhile (< (n+1)) primos)

sublistas :: [a] -> [[a]]
sublistas []      =  [[]]
sublistas (x:xs)  =  [ x:ys | ys <- sublistas xs ] ++ sublistas xs
 -}

{- type Par a = (a,a)
type NomeSinon = String
f :: NomeSinon -> Char
f n = head n

data Nome = N String 
g :: Nome -> Char
g (N s) = head s

data Par a = P (a,a)
data Arvore a = Folha a | No (Arvore a) a (Arvore a)
 -}

writeFoo = putStrLn "foo"

----------------------------
----------------------------
{-
main = imprimirStr
imprimirStr :: IO ()
imprimirStr = do putStrLn "primeira string"
                 putStrLn "segunda string"
--}

{--
put4times :: String -> IO ()
put4times str = do putStrLn str
                   putStrLn str
                   putStrLn str
                   putStrLn str
-}

{-                  
putNtimes :: Int -> String -> IO ()
putNtimes n str 
  = if n <= 1
       then putStrLn str
       else do putStrLn str
               putNtimes (n-1) str
--}

{--
--main = readLines
readLines :: IO ()
readLines =
 do getLine
    getLine
    putStrLn "Linhas lidas"

--}

{-
--main = getNput 
getNput :: IO ()
getNput = do line <- getLine
             putStrLn line
--}

{--
--main = reverse2lines
reverse2lines:: IO ()   
reverse2lines =
    do line1 <- getLine
       line2 <- getLine
       putStrLn (reverse line2)
       putStrLn (reverse line1)
--}

{-
main = do  
        return ()  
        return "HAHAHA"  
        line <- getLine  
        linha <- return "BLAH BLAH BLAH"  
        return 4  
        putStrLn line
        putStrLn linha
--}


{-
main = do   
    line <- getLine  
    if null line  
        then return () -- putStrLn " "
        else (do  
            putStrLn $ reverseWords line  
            main)   
reverseWords :: String -> String  
reverseWords = unwords . map reverse. words         

--}            

{-      
func :: IO String
func =  do 
         return "oi"
--}

{--
main = do  
        return ()  
        ln <- return "HAHAHA"  
        putStrLn ln
        line <- getLine  
        return "BLAH BLAH BLAH"  
        return 4 
        ln <- func
        putStrLn ln
        putStrLn line
--}

{-
main = do 
    a <- return "oi"
    b <- return "tchau"
    putStrLn $ a ++ " " ++ b 
--}

--{--
showStackHead []     = return []
showStackHead (x:xs) = do
   putStrLn $ "result: " ++ [x]
   return xs

main :: IO ()
main = do
   let s1 = []
   r1 <- showStackHead s1
   putStrLn $ "returned: " ++ r1
   putStrLn "---"

   let s2 = "foo"
   r2 <- showStackHead s2
   putStrLn $ "returned: " ++ r2

   putStrLn "---"

   let s3 = "q"
   r3 <- showStackHead s3
   putStrLn $ "returned: " ++ r3
   
--}  


 
{-
import Control.Monad

main = do line <- getLine
          when  (line /= []) $
             do putStrLn line
                main
--}

{-

main :: IO ()
main =
    do
        let xs = [2,3,4]
        xr <- dot xs
        xrr <- dot xr
        return ()

dot (x:xs) =
    do
        print x
        return xs
--}
