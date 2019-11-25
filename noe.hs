
logoskell2svg:: [(Int,Int)]-> (Int,Int)->[[Char]]->[[Char]]
logoskell2svg [] _ _ = []
logoskell2svg (x:xs) (cx,cy) s =
    t++logoskell2svg xs (nx,ny) t
    where nx =cx+fst x
          ny = cy+snd x
          t=[concat["<line x1=\"",show(cx) ,"\"y1=\"",show(cy),"\" x2=\"",show(nx),"\" y2=\"",show(ny),"\" stroke=\"red\" />\n"]]
    
decomplist:: [[Char]]->[Char]->[Char]
decomplist [] a = a
decomplist (x:xs) s = 
    s++x++decomplist xs s 

buildfile:: [Char]->[Char]
buildfile a = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"200\" height=\"200\">\n<title>Exemple</title>\n"++a++"</svg>"
