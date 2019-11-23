 import System.IO

-- utility
 getTuple :: (Int, Int, Int) -> Int -> Int
 getTuple (x,y,z) i = case i of
     0 -> x
     1 -> y
     2 -> z

-- fonction qui traite une instruction forward

 forward :: Int -> Int -> (Int, Int, Int)
 forward s l = (x, y, s)
     where r = fromIntegral s :: Float
           a = r * (pi/180)
           b = fromIntegral l :: Float
           x = round (b * cos a)
           y = round (b * sin a)

-- fonction qui traite une instruction de rotation

 turn :: Int -> Int -> (Int, Int, Int)
 turn old val = (0, 0, new)
    where new = old + val

-- fonction qui trie en fonction d'un caractere
 
 parseInst :: [[Char]] -> [Char] -> [[Char]]
 parseInst out [] = out
 parseInst out (x:xs)
     | x == "["!!0 = parseInst outRepeat next
     | x == ","!!0 = parseInst (out ++ [""]) xs
     | otherwise = parseInst (outWithoutTail ++ [(last out) ++ [x]]) xs
     where outWithoutTail = take (length out - 1) out
           next = reverse . takeWhile (/= "]"!!0) . reverse $ xs
           outRepeat = take (length out - 1) out ++ [((last out) ++ "[" ++ (takeWhile (/= "]"!!0) xs)) ++ "]"]

-- premier traitement

 encadrement :: [Char] -> [Char]
 encadrement (x:xs)
     | x == " "!!0 = encadrement xs
     | last xs == " "!!0 = encadrement ([x] ++ (take (length xs - 1) xs))
     | x == "["!!0 && last xs == "]"!!0 = take (length xs - 1) xs
     | otherwise = ""

-- déclaration d'une instruction comme un type
 
 data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Char]  deriving (Read, Show)

-- fonction qui traite l'instruction repeat
   
 parseRepeat :: [[Char]] -> [[Char]] -> [[Char]]
 parseRepeat [] out = out
 parseRepeat (x:xs) out = case (read x :: Instruction) of
     (Main.Repeat a b) -> parseRepeat (["Repeat " ++ show (a-1) ++ "[ " ++ b ++ " ]"] ++ xs) (parseInst out b) 
     _ -> parseRepeat xs out

--fonction qui redirige l'instruction sur la bonne fonction

-- compute :: (Floating a) => a ->  Instruction -> [(a, a, a)] 
 redirect s txt = case txt of
     (Forward x) -> forward s x
     (Main.Left x) -> turn s (-x)
     (Main.Right x) -> turn s x

-- fonction globale

 compute :: [(Int, Int)] -> [[Char]] -> Int -> [(Int, Int)]
 compute out [] _ = out
 compute out (x:xs) s = compute newOut xs newS 
    where outRedirect = redirect s (read x :: Instruction)
          newCoord = ((getTuple outRedirect 0), (getTuple outRedirect 1))
          newS = getTuple outRedirect 2
          newOut = if newCoord == (0, 0) then out else out ++ [newCoord]
 
 main = do
    let angle = 0 
    str <- hGetLine stdin
    putStr str
