module RayTransform where

type twoD = (Int, Int)
type threeD = (Int, Int, Int)

render canvas viewport camera = map paintPixel canvas
    where
        paintPixel point = setPixel point color
            where point' = canvasToViewport point
                  color  = trace origin point' (1,inf)

        canvasToViewport (x,y) = (x*vw/cw, y*vh/ch, d)

        (vw,vh) = (width viewport, height viewport)
        (cw,ch) = (width canvas, height canvas)

        d = distance viewport camera

