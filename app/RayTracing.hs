module RayTracing where

type twoD = (Int, Int)
type threeD = (Int, Int, Int)

data Sphere = Sphere { center :: threeD, radius :: Double , color :: Color}
data Color = Color { red :: Double, green :: Double, blue :: Double}

render canvas viewport camera = map paintPixel canvas
    where
        paintPixel point = setPixel point color
            where point' = canvasToViewport point
                  color  = trace origin point' (1,inf)

        canvasToViewport (x,y) = (x*vw/cw, y*vh/ch, d)

        (vw,vh) = (width viewport, height viewport)
        (cw,ch) = (width canvas, height canvas)

        d = distance viewport camera

trace origin dest (lbound, ubound) = loop closest_t closest_sphere spheres
    loop closest_t closest_sphere [] =
        case closest_sphere of
            Nothing -> bg_color
            Just s -> color s
    
    loop closest_t closest_sphere (sphere:spheres)
        | t1 in [t_min, t_max] and t1 < closest_t = loop t1 sphere spheres
        | t2 in [t_min, t_max] and t2 < closest_t = loop t2 sphere spheres
        where (t1,t2) = intersectRaySphere sphere

    intersectRaySphere sphere
        | discriminant < 0  = (inf,inf)
        | otherwise         = (t1,t2)
        where
            r = radius sphere
            co = vectorFrom (center sphere) origin

            a = dest `dot` dest
            b = 2 * (co `dot` dest)
            c = (co `dot` co) - (r * r)

            discriminant = (b * b) - (4 * a * c)

            t1 = (-b + (sqrt discriminant)) / (2 * a)
            t2 = (-b - (sqrt discriminant)) / (2 * a) 

