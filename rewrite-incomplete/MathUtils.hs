{-# LANGUAGE AllowAmbiguousTypes, NoMonomorphismRestriction #-}
module MathUtils where

-- to use this module, functions need to be constrained into Autofloat first

---- constants ----

eps = 0.1 ** 8
posInf = 1 / 0
negInf = -1 / 0

---- helper functions ----

eq a b = abs (a-b) < eps

isInf x = x == 1/0 || x == -1/0

neg v = map (\x->(-x)) v

add v1 v2 = map (\(a,b)->a+b) (zip v1 v2)

sub v1 v2 = v1 `add` (neg v2)

rot90 [x,y] = [-y,x]

-- scalar-vector multiplication
mult k v = map (k*) v

magsq xs = foldl (+) 0.0 $ map (**2) xs

mag xs = sqrt $ magsq xs

normalize xs = let magnitude = mag xs in
    map (/magnitude) xs

dot v1 v2 = let zp = zip v1 v2 in foldl (+) 0 $ map (\(a,b)->a*b) zp

lerp v1 v2 k = let 
    lerpNum (a,b) = a*(1.0-k) + b*k
    in map lerpNum $ zip v1 v2

-- transformations

centroid pts = let
    len = fromIntegral $ length pts
    [sumx, sumy] = foldl add [0,0] pts
    in [sumx/len, sumy/len]

combTrans [mx1,my1,r1,s1] [mx2,my2,r2,s2] = 
    [mx1+mx2, my1+my2, r1+r2, s1*s2]

negTrans [mx,my,r,s] = [-mx,-my,-r,1/s]

transformP [x,y] [mx,my,t,s] = let
    scost = s * (cos t)
    ssint = s * (sin t)
    x' = x*scost - y*ssint + mx
    y' = x*ssint + y*scost + my
    in [x', y']

transformG poly amt = map (\p->transformP p amt) poly

