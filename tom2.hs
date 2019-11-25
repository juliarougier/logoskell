import System.IO
import Control.Monad
import Data.Char

data Instruction = Forward Float | Left Float | Right Float | Repeat Int [Instruction] deriving    (Read, Show)

forward :: Float -> Float -> (Float, Float)
forward s l = (x, y)
     where a = s * (pi/180)
           x = l * cos a
           y = l * sin a

-- fonction qui traite une instruction de rotation
 
turn :: Float -> Float -> Float
turn old val = new
     where new = old + val


-- fonction qui compute l'ensemble des instructions pour en faire un programme entier
compute :: [Instruction] -> [(Float, Float)] -> Float -> [(Float, Float)]
compute [] out _ = out
compute (x:xs) out s = case x of
    (Main.Forward i) -> compute xs (out ++ [forward s i]) s
    (Main.Left i) -> compute xs out (turn s (-i))
    (Main.Right i) -> compute xs out (turn s i)
    (Main.Repeat i j) -> compute newXs out s
        where newXs = (take ((length j) * i) (cycle j)) ++ xs

main = do
     contents <- hGetLine stdin
     -- ligne qui utilse toute les fonctions pour extraire
     let out = compute (read contents :: [Instruction]) [] 0
     hPutStr stdout (show out)

