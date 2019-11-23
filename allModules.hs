import Prelude

-- fonction qui trie en fonction d'un caractere

parse :: [[Char]] -> [Char] -> [Char] -> [[Char]]
parse out _ [] = out
parse out c (x:xs)
    | x == c!!0 = parse (out ++ [""]) c xs
    | otherwise = parse (outWithoutTail ++ [(last out) ++ [x]]) c xs
    where outWithoutTail = take (length out - 1) out


 -- fonction qui supprime la chaine de caractère du premier parametre dans le     deuxieme

removeChain :: [Char] -> [Char] -> [Char]
removeChain _ "" = ""
removeChain c (x:xs)
    | x == c!!0 = removeChain c xs
    | otherwise = [x] ++ (removeChain c xs)

-- fonction qui enleve un caractère au debut et à la fin

removeFirstAndLast :: [Char] -> [Char] -> [Char]
removeFirstAndLast c (x:xs)
    | x == c!!0 = removeFirstAndLast c xs
    | (last xs) == c!!0 = [x] ++ (take (length xs - 1) xs)
    | otherwise = out
    where out = [x] ++ xs


-- fonction qui trie en fonction d'un caractere
   
parseInst :: [[Char]] -> [Char] -> [[Char]]
parseInst out [] = out
parseInst out (x:xs)
    | x == "["!!0 = parseInst outRepeat next
    | x == ","!!0 = parseInst (out ++ [""]) xs
    | otherwise = parseInst (outWithoutTail ++ [(last out) ++ [x]]) xs
    where outWithoutTail = take (length out - 1) out
          next = reverse . takeWhile (/= "]"!!0) . reverse $ xs
          outRepeat = take (length out - 1) out ++ [((last out) ++ (takeWhile (/= "]"!!0) xs))]


