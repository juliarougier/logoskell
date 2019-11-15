--parse :: [[Char]] -> [Char] -> [Char] -> [[Char]]
parse _ _ [] = []
parse out c (x:xs)
    | x == c!!0 = parse (out ++ [""]) c xs
    | otherwise = parse (outWithouTtail ++ [last out ++ [x]]) c xs
        where outWithouTtail = take (length out - 1) out