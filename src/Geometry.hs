module Geometry where

import Prelude hiding (length)

type Vector = [Double]

length v = sqrt (v `dot` v)

c `scale` v = map (*c) v

normalize v@[0,0,0] = v
normalize v = (1 / length v) `scale` v

v `dot` w = sum $ zipWith (*) v w

v `projectedOnto` w = (v `dot` normalize w) `scale` normalize w

v `cosAngleBetween` w = normalize v `dot` normalize w

v `orthogonal` w = (v `dot` w)  == 0

[a, b, c] `cross` [x, y, z] = [b*z-c*y, c*x-a*z, a*y-b*x]

v `plus` w = zipWith (+) v w

v `minus` w = zipWith (-) v w

type Matrix = [[Double]]

rows = id
cols m = [map (!!c) m | (c,_)<-zip [0..] (head m)]

a `multiply` b = [[c `dot` r | c<- cols b] | r<-rows a]

m `transform` v = map head $ m `multiply` map (:[]) v

epsilon n = replicate n 0 ++ [1] ++ repeat 0

-- homogenous points

mkPoint a b c = [a,b,c,1]

m `transformP` p = 
    let p' = m `transform` p
    in case last p' of
        0  -> p'
        w' -> (1/w') `scale` p'

mkVec a b c = [a,b,c,0]                  