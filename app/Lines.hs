module Lines where

data Point = Point {x::Double, y::Double}

to = ($)

linefrom (Point {x0,y0}) (Point {x1,y1}) = 
    let line    = (round x0, round y0) : map f line
        f (x,y) = (x+1, y+a)
        a       = (y1-y0) / (x1-x0)
    in takeWhile (<= round x1 . fst) line

drawline p0 p1 color = 
    let line = linefrom p0 `to` p1
    in mapM (putpixel color) line

