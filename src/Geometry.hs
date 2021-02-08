module Geometry where

type Vector = [Double]

length v = sqrt (v `dot` v)

scale c v = map (*c) v

normalize v@[0,0,0] = v
normalize v = (1 / length v) `scale` v

v `dot` w = sum $ zipWith (*) v w

v `projected_onto` w = (v `dot` normalize w) `scale` normalize w

v `cos_angle_between` w = normalize v `dot` normalize w

v `orthogonal` w = (v `dot` w)  == 0

[a, b, c] `cross` [x, y, z] = [b*z-c*y, c*x-a*z, a*y-b*x]

v `plus` w = zipWith (+) v w

v `minus` w = zipWith (-) v w

type Matrix = [[Double]]

rows = id
cols m = [map (!!c) m | (c,_)<-zip [0..] (m!!0)]

a `multiply` b = [[c `dot` r | c<- cols b] | r<-rows a]