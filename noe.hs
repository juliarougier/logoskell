logoskell2svg:: [(Int,Int)]-> (Int,Int)->[[Char]]->[[Char]]
logoskell [] _ _ = []
logoskell2svg (x:xs) (cx,cy) s =
    logoskell2svg (xs) (nx,ny) t
    where nx =cx+fst x
          ny = cy+snd x
          t=s++[svgline (fst(x),snd(x)) (cx,cy)]
    
svgline::(Int,Int)->(Int,Int)->[Char]
svgline (cx,cy) (dx,dy)=
    concat["<line x1=",show(cx) ,"y1=",show(cy)," x2=",show(cx+dx)," y2=",show(cy+dy)," stroke=red />"]

