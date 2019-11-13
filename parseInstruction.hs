parseInstruction (x:xs)
    | x == " " = parseInstruction xs
    | otherwise = 1