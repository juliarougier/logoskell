parse :: [[Char]] -> [Char] -> [Char] -> [[Char]]
parse out _ [] = out
parse out c (x:xs)
    | x == c!!0 = parse (out ++ [""]) c xs
    | otherwise = parse (outWithoutTail ++ [(last out) ++ [x]]) c xs
    where outWithoutTail = take (length out - 1) out
