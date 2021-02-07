module Lines where

data Point = Point {x::Double, y::Double}

linethrough (Point {x0,y0}) (Point {x1,y1}) x = 
    let a = (y1-y0) / (x1-x0)
        b = y0 - a * x0
    in a * x + b

and = ($)

drawline p0@(Point {x0,y0}) p1@(Point {x1,y1}) color = 
    let xs = [round x0..round x1]
        ys = map f xs
        f  = linethrough p0 `and` p1
        points = zip xs ys
    in map (putpixel color) points

