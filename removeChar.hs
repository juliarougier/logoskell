-- methode qui supprime la chaine de caractère du premier parametre dans le deuxieme

removeChar :: [Char] -> [Char] -> [Char]
removeChar _ "" = ""
removeChar c (x:xs)
    | x == c!!0 = removeChar c xs
    | otherwise = [x] ++ (removeChar c xs)