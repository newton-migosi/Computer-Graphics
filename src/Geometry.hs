module Geometry where

data Vector = Vector { x_coord:: Double, y_coord:: Double , z_coord:: Double }

length (Vector x y z) = sqrt (x*x + y*y + z*z)

scale c (Vector x y z) = Vector (c*x) (c*y) (c*z)

normalize v@(Vector 0 0 0) = v
normalize v@(Vector x y z) = (1 / vector_length v) `scale` v

(Vector a b c) `dot` (Vector x y z) = a*x + b*y + c*z

v `projected_onto` w = (v `dot` normalize w) `scale` normalize w

v `cos_angle_between` w = normalize v `dot` normalize w

v `orthogonal` w = (v `dot` w)  == 0

(Vector a b c) `cross` (Vector x y z) = Vector (b*z-c*y) (c*x-a*z) (a*y-b*x)

(Vector a b c) `plus` (Vector x y z) = Vector (a+x) (b+y) (c+z)

v `minus` w = v `plus` (-1 `scale` w)

