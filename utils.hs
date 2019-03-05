module Utils where

import PointsAndLines
import Polygons

type Vect = (Double, Double)
type Angle = Double

-- (helper) get value from a Maybe Double, default to 0
valof x = case x of Nothing -> 0
                    Just x -> x

valof' dim x = case x of Nothing -> take dim [0,0..]
                         Just x -> x

-- (helper) gives negation of a vector
neg :: Vect -> Vect
neg (a,b) = (-a,-b)

-- (helper) generalized neg
neg' :: [Double] -> [Double]
neg' v = map (\x->(-x)) v

-- (helper) adds two vector together
add :: Vect -> Vect -> Vect
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

add' :: [Double] -> [Double] -> [Double]
add' v1 v2 = map (\(a,b)->a+b) (zip v1 v2)

-- (helper) does vector subtraction
minus :: Vect -> Vect -> Vect
minus (x1,y1) (x2,y2) = (x1-x2, y1-y2)

-- (helper) scalar-vector multiplication
mult' :: Double -> [Double] -> [Double]
mult' k v = map (\a->k*a) v

-- (helper) gives mag of vector
mag :: Vect -> Double
mag (x,y) = sqrt $ x**2 + y**2

-- (helper) generalized mag
mag' :: [Double] -> Double
mag' xs = sqrt $ foldl (\a b->a+b) 0 (map (\x->x**2) xs)

-- (helper) normalizes a vector
normalize :: Vect -> Vect
normalize (a,b) = let
  len = dist (0,0) (a,b)
  in (a/len, b/len)

-- (helper) generalized normalize
normalize' :: [Double] -> [Double]
normalize' xs = let magnitude = mag' xs in
  map (\x -> x/magnitude) xs

-- (helper) scale vector by a factor
scale :: Double -> Vect -> Vect
scale s (a,b) = (a*s, b*s)

-- (helper) gives unit normal of segment xy
segN ((x1,y1),(x2,y2)) = let
  len = dist (x1,y1) (x2,y2)
  in (-(y2-y1)/len, (x2-x1)/len)
  
-- (helper) gives signed dist bt. p, xy 
-- case 3 pos, case 4 neg, see gradients doc
sdistPL :: Point -> LineSeg -> Double
sdistPL (p1,p2) ((x1,y1),(x2,y2)) = let
  (u1, u2) = segN ((x1,y1),(x2,y2))
  in u1 * (p1-x1) + u2 * (p2-y1)

-- (helpers below) returns elements transformed by some amount

transformP' :: Point -> Point -> [Double] -> Point
transformP' (x,y) (cx,cy) [mx,my,t,s] = let
  scost = s * (cos t)
  ssint = s * (sin t)
  x' = x*scost - y*ssint + scost*(mx-cx) - ssint*(my-cy) + cx
  y' = x*ssint + y*scost + ssint*(mx-cx) + scost*(my-cy) + cy
  in (x', y')

transformP :: Point -> [Double] -> Point
transformP (x,y) [mx,my,t,s] = let
  scost = s * (cos t)
  ssint = s * (sin t)
  x' = x*scost - y*ssint + mx
  y' = x*ssint + y*scost + my
  in (x', y')

transformG :: Polygon -> [Double] -> Polygon
transformG poly amt = map (\p->transformP p amt) poly

movebyPm :: Point -> Vect -> Point
movebyPm (x,y) (mx,my) = (x+mx, y+my)

movebySm :: LineSeg -> Vect -> LineSeg
movebySm (x,y) m = (movebyPm x m, movebyPm y m)

movebyGm :: Polygon -> Vect -> Polygon
movebyGm pts m = map (\p->movebyPm p m) pts

rotateAroundPSa :: Point -> LineSeg -> Angle -> LineSeg
rotateAroundPSa c (p1,p2) psi = (rotateAroundPPa c p1 psi, rotateAroundPPa c p2 psi)

rotateAroundPPa :: Point -> Point -> Angle -> Point
rotateAroundPPa (c1,c2) (p1,p2) psi = let
  l = dist (c1,c2) (p1,p2)
  theta = atan2 (p2-c2) (p1-c1)
  costheta = cos (theta+psi)
  sintheta = sin (theta+psi)
  in (c1 + l*costheta, c2 + l*sintheta)

rotateAroundPGa :: Point -> Polygon -> Angle -> Polygon
rotateAroundPGa c poly psi = map (\p->rotateAroundPPa c p psi) poly

scalePPk :: Point -> Point -> Double -> Point
scalePPk (c1, c2) (p1, p2) k = (c1 + k*(p1-c1), c2 + k*(p2-c2))

scalePSk :: Point -> LineSeg -> Double -> LineSeg
scalePSk c (x, y) k = (scalePPk c x k, scalePPk c y k)

scalePGk :: Point -> Polygon -> Double -> Polygon
scalePGk c poly k = map (\p -> scalePPk c p k) poly

cent :: Polygon -> Point
cent poly = let
  (sumx, sumy) = foldl (\(a,b) (x,y)->(a+x, b+y)) (0,0) poly
  len = fromIntegral $ length poly
  in (sumx / len, sumy / len)