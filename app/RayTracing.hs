module RayTracing where

import Light (computeLight)

type twoD = (Int, Int)
type threeD = (Int, Int, Int)

data Sphere = Sphere { center :: threeD, radius :: Double , color :: Color, material :: Material}
data Color = Color { red :: Double, green :: Double, blue :: Double}
data Material = Matte | Shiny { specularity :: Double}

data Scene = Scene { lights :: [Light], cameras::[Camera], viewport :: Viewport, spheres :: [Sphere]}

render canvas viewport camera = map paintPixel canvas
    where
        paintPixel point = setPixel point color
            where point' = canvasToViewport point
                  color  = trace origin point' (1,inf)

        canvasToViewport (x,y) = (x*vw/cw, y*vh/ch, d)

        (vw,vh) = (width viewport, height viewport)
        (cw,ch) = (width canvas, height canvas)

        d = distance viewport camera

trace origin dest (lbound, ubound) = trace' color recursion_depth
    where
        trace' color recursion_depth =
            let (closest_sphere', closest_t') = 
                    closest_intersection closest_t closest_sphere spheres 
            in  case closest_sphere of
                    Nothing -> bg_color
                    Just sphere -> 
                        let point       = origin `vectorPlus` closest_t * d
                            orthogonal  = vectorFrom point `to` center sphere
                            orthonormal = orthogonal `div` vectorSize orthogonal
                            intensity   = computeLight point orthonormal (flip dest) (material sphere)
                            local_color'= color sphere * intensity
                        in case reflective sphere of
                             Nothing    -> local_color
                             Just r     -> 
                                let r = reflected_ray (flip dest)  orthonormal
                                    reflected_color = trace point r (0.001, inf)
                                in local_color * (1-r) + reflected_color * r
                                

    

    closest_intersection closest_t closest_sphere []  = (closest_sphere, closest_t)
            
    closest_intersection closest_t closest_sphere (sphere:spheres)
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

