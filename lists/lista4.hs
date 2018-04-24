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

testaLista1 :: (a -> Bool) -> [a] -> Bool
testaLista1 f [] = True
testaLista1 f (x : xs)
    | f x == True = testaLista1 f xs
    | f x == False = False

testaLista2 :: (a -> Bool) -> [a] -> Bool
testaLista2 f xs = and (map f xs)

testaLista3 :: (a -> Bool) -> [a] -> Bool
testaLista3 p = foldr (\ x -> (p x &&)) True

type Nome = String
type Conteudo = String
data File = ArqSimples Nome Conteudo | Diretorio Nome [File]

instance Eq File where
    (ArqSimples n1 c1) == (ArqSimples n2 c2) = n1 == n2 && c1 == c2
    (Diretorio n1 arqs1) == (Diretorio n2 arqs2) = n1 == n2 && arqs1 == arqs2
    _ == _ = False -- Anything that is not one of the two options above means that they are not equal
                   -- therefore, it is False

instance Show File where
    show (ArqSimples n1 c1) = show n1
    show (Diretorio n1 arqs1) = show n1 ++ " " ++ show arqs1

f1, f2, f3 :: File 
f1 = ArqSimples "config.sys" "xyzxyz"  
f2 = Diretorio "Windows" [ArqSimples "win.ini" "xxx",
                          ArqSimples "win.exe" "sss"] 
f3 = Diretorio "Windows" [ArqSimples "win.ini" "xxx", 
                          Diretorio "Foo" [ArqSimples "kkk.exe" "kbum"]]
fileSys :: [File] 
fileSys = [ArqSimples   "config.sys" "xyzxyz", 
           ArqSimples   "autoexec.bat" "bbb", 
           Diretorio  "Windows" [ArqSimples   "win.ini" "xxx", 
                                ArqSimples   "win.exe" "sss",
                                Diretorio  "maisUmDir" []
                               ], 
           ArqSimples   "autoexec.bak" "rrr" 
          ]
fileSys2 :: [File] 
fileSys2 = [ Diretorio  "Windows" [ArqSimples   "win.ini" "xxx", 
             Diretorio  "etc" [Diretorio  "h"[]]  ,
             Diretorio  "maisUmDir" []] 
           ]

getName :: File -> Nome
getName (ArqSimples n _) = n
getName (Diretorio n _) = n

getSubDir :: File -> [File]
getSubDir (Diretorio _ xs) = xs
getSubDir _ = []

isDir :: File -> Bool
isDir (Diretorio _ _) = True
isDir _ = False

dirExist :: [File] -> Nome -> Bool
dirExist [] _ = False
dirExist (x : xs) n
    | isDir x && getName x == n = True
    | otherwise = dirExist xs n || dirExist (getSubDir x) n

cd :: [File] -> Nome -> Nome
cd f n
    | dirExist f n = n
    | otherwise = "Diretorio nao existe"

getAllSimpleFiles :: [File] -> [Nome]
getAllSimpleFiles [] = []
getAllSimpleFiles ((ArqSimples n _) : xs) = n : getAllSimpleFiles xs
getAllSimpleFiles ((Diretorio _ x) : xs) = getAllSimpleFiles x ++ getAllSimpleFiles xs

fileExist :: [File] -> Nome -> Bool
fileExist [] _ = False
fileExist (x : xs) n
    | getName x == n = True
    | otherwise = fileExist xs n || fileExist (getSubDir x) n

mkdir :: [File] -> Nome -> [File]
mkdir (x : xs) n
    | (fileExist (x : xs) n) = (x : xs)
    | otherwise = (x : xs) ++ [Diretorio n []]