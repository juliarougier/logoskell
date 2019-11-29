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

-- fonction qui retourne une liste de traits au format xml/svg au format liste de String
-- Cette fonction prends en entrée la liste des deplacements du crayon restants à effectuer, la position courante du crayon (cx,cy), et la liste des traits déjà ecrits au format SVG
logoskell2svg:: [(Float,Float)]-> (Float,Float)->[[Char]]->[[Char]]
logoskell2svg [] _ _ = []
logoskell2svg (x:xs) (cx,cy) s =
   t++logoskell2svg xs (nx,ny) t
   where nx =cx+fst x
         ny = cy+snd x
         t=[concat["<line x1=\"",show(cx) ,"\" y1=\"",show(cy),"\" x2=\"",show(nx),"\" y2=\"",show(ny),"\" stroke=\"red\" />\n"]]

-- fonction qui transforme la liste des traits au format liste de String,  en une seule Sting 
decomplist:: [[Char]]->[Char]->[Char]
decomplist [] a = a
decomplist (x:xs) s = 
    s++x++decomplist xs s 

-- fonction qui ajoute l'en-tete et le pied de page au fichier pour respecter la syntaxe xml à la sting contenant l'ensemble des traits
buildfile:: [Char]->String
buildfile a = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"500\" height=\"500\">\n<title>Exemple</title>\n"++a++"</svg>"        

main = do
     --Recuperation des instructions logoskell dans une variable
     contents <- hGetLine stdin
     
     -- 
     hPutStr stdout (buildfile (decomplist (logoskell2svg (compute (read contents :: [Instruction]) [] 0) (250,250) [""]) ""))
