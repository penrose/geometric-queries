module Gradients (
  -- one segment
  movepPS,
  movexyPS,
  rotxyPSTout,
  rotxyPSCout,
  -- two segments
  movepPSS,
  movexyPSS,
  rotxyPSSCout,
  -- a point and a polygon
  rotbPGCout,
  -- two segments (rotate B to get close to A)
  rotbSSCout,
  -- a segment and a polygon
  rotbSGCout,
  -- two polygons
  rotbGGCout,
  -- graphing
  graphDistPsiPSC,
  graphDist2PsiPSC,
  graphDistPsiPGC,
  graphDistPsiSSC,
  graphDistPsiSGC,
  graphDistPsiGGC
) where

import Numeric.AD
import PointsAndLines
import Polygons
import Linesearch

type Vect = (Double, Double)
type Angle = Double

segCase :: Point -> LineSeg -> Int
segCase p (x,y) = let
  p' = closestPointPS p (x,y)
  in if isEndpoint p' (x,y) then let
    pxsqr = distsq p x
    pysqr = distsq p y
    in if pxsqr<pysqr then 1 else 2
  else let
    sd = sdistPL p (x,y)
    in if sd>=0 then 3 else 4

distPS :: Point -> LineSeg -> Double
distPS p (x,y) = case segCase p (x,y) of
  1 -> dist p x
  2 -> dist p y
  3 -> sdistPL p (x,y)
  4 -> -(sdistPL p (x,y))

-- (output)
movepPS :: Point -> LineSeg -> (Vect, Double)
movepPS p (x,y) = let 
  d = distPS p (x,y)
  stepDen = 20
  in case segCase p (x,y) of
  1 -> (scale (1/stepDen) $ minus p x, d)
  2 -> (scale (1/stepDen) $ minus p y, d)
  3 -> (scale (1/stepDen) $ scale d $ segN (x,y), d)
  4 -> (scale (1/stepDen) $ scale d $ neg $ segN (x,y), d)

-- (output)
movexyPS :: Point -> LineSeg -> (Vect, Double)
movexyPS p (x,y) = let (res, d) = movepPS p (x,y)
  in (neg res, d)

-- (output) given p and xy, gives next state & grad (for conv check)
rotxyPSTout :: Point -> LineSeg -> (LineSeg, Double)
rotxyPSTout pt xy = let
  ct = 0.5--maxRotCentPS pt xy
  psi = rotxyPST pt xy ct
  stepDen = 6 * 10**4
  a = -psi/stepDen
  xy' = rotateAroundPSa (fromT xy ct) xy (constrain (-0.05) 0.05 a)
  in (xy', psi)

maxRotCentPS :: Point -> LineSeg -> Double
maxRotCentPS pt xy = let
  closest = closestPointPS pt xy
  qt = getT xy closest
  in if qt<0.5 then 1 else 0

-- calculates const * grad of change in dist from p to xy wrt.
-- rotation of xy around c corresponding to given t
rotxyPST :: Point -> LineSeg -> Double -> Angle
rotxyPST pt xy t
  | case1 = let
      theta = a_xp - a_xy
      in - (sin theta) * xc * xp
  | case2 = let
      theta = a_yp - a_xy
      in - (sin theta) * yc * yp
  | case3 = --if qt < t then cq else -cq 
    let
      theta = atan2 qp cq
      in if qt < t then pc**2 * (cos theta) * (sin theta) 
         else -pc**2 * (cos theta) * (sin theta)
  | case4 = --if qt < t then -cq else cq
    let
      theta = atan2 qp cq
      in if qt < t then -pc**2 * (cos theta) * (sin theta)
         else pc**2 * (cos theta) * (sin theta)
  where cs = segCase pt xy
        -- cases
        case1 = cs == 1
        case2 = cs == 2
        case3 = cs == 3
        case4 = cs == 4
        -- unwrap bindings
        (x,y) = xy
        ((x1,y1), (x2,y2)) = xy
        (p1,p2) = pt 
        -- for case 1 & 2
        a_xy = atan2 (y2-y1) (x2-x1)
        a_xp = atan2 (y1-p2) (x1-p1)
        a_yp = atan2 (p2-y2) (p1-x2)
        c = fromT xy t
        xc = dist x c
        yc = dist y c
        xp = dist x pt
        yp = dist y pt
        -- for case 3 & 4
        pc = dist c pt
        qt = getT xy pt
        q = fromT xy qt
        cq = dist c q
        qp = dist pt q
  
-- (output)
rotxyPSCout :: Point -> LineSeg -> Point -> (LineSeg, Double)
rotxyPSCout pt xy c = let
  rotxy = rotateAroundPSa c xy
  f x = (distPS pt $ rotxy x)**2
  g x = rotxyPSC pt (rotxy x) c
  psi = rotxyPSC pt xy c
  stepDen = 20 * 10**4 -- maybe relate this to scale of elements can fix oscillation?
  a = -psi/stepDen
  xy' = rotateAroundPSa c xy (constrain (-0.05) 0.05 a)
  in (xy', psi)

a = (404.0,216.0)
b = ((324.0,364.0), (527.0,319.0))
c = (442.0,425.0)

rotxyPSC :: Point -> LineSeg -> Point -> Angle
rotxyPSC pt xy c
  | case1 = let 
      theta = a_xp - a_xc
      in - (sin theta) * xc * xp
  | case2 = let
      theta = -a_yp + a_yc
      in - (sin theta) * yc * yp
  | case3 = if qt < ct then qp * c'q else -qp * c'q
  | case4 = if qt < ct then -qp * c'q else qp * c'q
  where cs = segCase pt xy
        -- cases
        case1 = cs == 1
        case2 = cs == 2
        case3 = cs == 3
        case4 = cs == 4
        -- unwrap bindings
        (x,y) = xy
        ((x1,y1), (x2,y2)) = xy
        (p1,p2) = pt
        (c1,c2) = c
        -- for case 1 & 2
        a_xc = atan2 (c2-y1) (c1-x1)
        a_xp = atan2 (y1-p2) (x1-p1)
        a_yc = atan2 (c2-y2) (c1-x2)
        a_yp = atan2 (p2-y2) (p1-x2)
        xc = dist x c
        xp = dist x pt
        yc = dist y c
        yp = dist y pt
        -- for cases 3 & 4
        qt = getT xy pt
        q = fromT xy qt
        ct = case segCase c xy of
          1 -> 0
          2 -> 1
          _ -> getT xy c -- fix!!
        c' = fromT xy ct
        qp = dist pt q
        c'q = dist c' q

---------- Below: a point and two segments (prep for polygons) ------------

distPSS :: Point -> LineSeg -> LineSeg -> Double
distPSS p seg1 seg2 = let
  d1 = distPS p seg1
  d2 = distPS p seg2
  in min d1 d2

movepPSS :: Point -> LineSeg -> LineSeg -> (Vect, Double)
movepPSS p seg1 seg2
  | closer1 = movepPS p seg1
  | otherwise = movepPS p seg2
  where closer1 = distPS p seg1 < distPS p seg2

movexyPSS :: Point -> LineSeg -> LineSeg -> (Vect, Double)
movexyPSS p seg1 seg2 = let (res, d) = movepPSS p seg1 seg2
  in (neg res, d)

rotxyPSSC pt seg1 seg2 c
  | closer1 = rotxyPSC pt seg1 c
  | otherwise = rotxyPSC pt seg2 c
  where closer1 = distPS pt seg1 < distPS pt seg2

rotxyPSSCout pt seg1 seg2 c = let
  psi = rotxyPSSC pt seg1 seg2 c
  stepDen = 6 * 10**5
  a = -psi/stepDen
  seg1' = rotateAroundPSa c seg1 (constrain (-0.05) 0.05 a)
  seg2' = rotateAroundPSa c seg2 (constrain (-0.05) 0.05 a)
  in ((seg1',seg2'), psi)

---------- Below: a point and a polygon ------------

distPG :: Point -> Polygon -> Double
distPG p poly = let
  ds = map (distPS p) (getSegments poly)
  in foldl min (ds!!0) ds

rotbPGC :: Point -> Polygon -> Point -> Angle
rotbPGC pt poly c = let
  polySegments = getSegments poly
  closest = foldl (\s1 s2->if distPS pt s1 < distPS pt s2 then s1 else s2)
    (polySegments!!0) polySegments
  in rotxyPSC pt closest c

rotbPGCout :: Point -> Polygon -> Point -> (Polygon, Double)
rotbPGCout pt poly c = let
  psi = rotbPGC pt poly c
  stepDen = 6 * 10**5
  a = -psi/stepDen
  poly' = rotateAroundPGa c poly (constrain (-0.05) 0.05 a)
  in (poly', psi)

---------- Below: two segments ------------

rotbSSC :: LineSeg -> LineSeg -> Point -> Angle
rotbSSC segA segB c = let
  -- p is the point on A closest to B
  p = closestPointSS segB segA
  in rotxyPSC p segB c

rotbSSCout :: LineSeg -> LineSeg -> Point -> (LineSeg, Double)
rotbSSCout segA segB c = let
  psi = rotbSSC segA segB c
  stepDen = 6 * 10**5
  a = -psi/stepDen
  segB' = rotateAroundPSa c segB (constrain (-0.05) 0.05 a)
  in (segB', psi)

---------- Below: a segment and a polygon ------------

rotbSGC :: LineSeg -> Polygon -> Point -> Angle
rotbSGC seg poly c = let
  polySegments = getSegments poly
  closest = foldl (\s1 s2 ->
    if shortestDistSS seg s1 < shortestDistSS seg s2 then s1
    else s2) (polySegments!!0) polySegments
  in rotbSSC seg closest c

rotbSGCout :: LineSeg -> Polygon -> Point -> (Polygon, Double)
rotbSGCout seg poly c = let
  psi = rotbSGC seg poly c
  stepDen = 6 * 10**5
  a = -psi/stepDen
  poly' = rotateAroundPGa c poly (constrain (-0.05) 0.05 a)
  in (poly', psi)

---------- Below: two polygons ------------

rotbGGC :: Polygon -> Polygon -> Point -> Angle
rotbGGC polyA polyB c = let
  aSegments = getSegments polyA
  closest2B = foldl (\s1 s2 ->
    if shortestDistGS polyB s1 < shortestDistGS polyB s2 then s1
    else s2) (aSegments!!0) aSegments
  in rotbSGC closest2B polyB c

rotbGGCout :: Polygon -> Polygon -> Point -> (Polygon, Double)
rotbGGCout polyA polyB c = let
  psi = rotbGGC polyA polyB c
  stepDen = 6 * 10**5
  a = -psi/stepDen
  polyB' = rotateAroundPGa c polyB (constrain (-0.05) 0.05 a)
  in (polyB', psi)

---------- Below: helpers (vector math, etc.) ------------

-- (output for graphing) (1 segment)
graphDistPsiPSC :: Point -> LineSeg -> Point -> ([Double], Double)
graphDistPsiPSC pt xy c = let
  den = 100
  angles = map (\i->2*pi*i/den - pi) [0..(den-1)]
  xys = map (rotateAroundPSa c xy) angles
  ds = map (\seg-> (distPS pt seg)**2) xys
  in (ds, foldl max 0 ds)

-- (output for graphing) (2 segments)
graphDist2PsiPSC :: Point -> LineSeg -> LineSeg -> Point -> ([Double], Double)
graphDist2PsiPSC pt seg1 seg2 c = let
  -- some weird type error makes me specify this two times?
  den = 100
  den2 = 100
  l = [0..(den-1)]
  angles = map (\i->2*pi*i/den - pi) l
  seg1s = map (rotateAroundPSa c seg1) angles
  seg2s = map (rotateAroundPSa c seg2) angles
  ds = map (\i -> (distPSS pt (seg1s!!i) (seg2s!!i))**2) [0..(den2-1)]
  in (ds, foldl max 0 ds)

-- (output for graphing) (polygon)
graphDistPsiPGC pt poly c = let
  den = 100
  angles = map (\i->2*pi*i/den - pi) [0..(den-1)]
  polys = map (rotateAroundPGa c poly) angles
  ds = map (\g -> (distPG pt g)**2) polys
  in (ds, foldl max 0 ds)

-- (output for graphing) (two segments)
graphDistPsiSSC segA segB c = let
  den = 100
  angles = map (\i->2*pi*i/den - pi) [0..(den-1)]
  segBs = map (rotateAroundPSa c segB) angles
  ds = map (\s -> (shortestDistSS segA s)**2) segBs
  in (ds, foldl max 0 ds)

-- (output for graphing) (a segment and a polygon)
graphDistPsiSGC seg poly c = let
  den = 100
  angles = map (\i->2*pi*i/den - pi) [0..(den-1)]
  polys = map (rotateAroundPGa c poly) angles
  ds = map (\g -> (shortestDistGS g seg)**2) polys
  in (ds, foldl max 0 ds)

-- (output for graphing) (two polygons)
graphDistPsiGGC polyA polyB c = let
  den = 100
  angles = map (\i->2*pi*i/den - pi) [0..(den-1)]
  polyBs = map (rotateAroundPGa c polyB) angles
  ds = map (\g -> (unsignedDistGG g polyA)**2) polyBs
  in (ds, foldl max 0 ds)

-- (helper) given an angle, returns its equiv. in range [0, 2*pi]
wrap2pi :: Angle -> Angle
wrap2pi theta = 
  if theta > -pi && theta <= pi then theta
  else if theta > pi then wrap2pi (theta - pi)
  else wrap2pi (theta + pi)

constrain :: Double -> Double -> Double -> Double
constrain lo hi val = 
  if val<lo then lo else
  if val>hi then hi
  else val

-- (helper) gives negation of a vector
neg :: Vect -> Vect
neg (a,b) = (-a,-b)

-- (helper) does vector subtraction
minus :: Vect -> Vect -> Vect
minus (x1,y1) (x2,y2) = (x1-x2, y1-y2)

-- (helper) normalizes a vector
normalize :: Vect -> Vect
normalize (a,b) = let
  len = dist (0,0) (a,b)
  in (a/len, b/len)

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

-- (helpers below) returns elements rotated around c for angle psi
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

------ below are just scratch code. Ignore for now...

-- a few functions to see what ad does
test [x] = tan x
test1 [x] = 1/x
test2 [x] = abs x
test3 [x] = if x<0 then -x-1 else x+1
test5 [x] = atan x

distfunc p1 p2 x1 y1 x2 y2 = let
  len = sqrt $ (y2-y1)**2 + (x2-x1)**2
  (u1, u2) = (-(y2-y1)/len, (x2-x1)/len)
  in abs $ u1 * (p1-x1) + u2 * (p2-y1)

movexy p1 p2 x1 y1 x2 y2 [v1, v2] = let
  x1' = x1 + v1
  y1' = y1 + v2
  x2' = x2 + v1
  y2' = y2 + v2
  in distfunc p1 p2 x1' y1' x2' y2'

{- need Autofloat to compile. Not important yet
test4 p1 p2 x1 y1 x2 y2 = let 
  f = movexy p1 p2 x1 y1 x2 y2
  in grad f [0, 0]
-}

rotxyc p1 p2 x1 y1 x2 y2 c1 c2 [psi] = let
  theta1 = atan $ (y1-c2)/(x1-c1)
  theta2 = atan $ (y2-c2)/(x2-c1)
  r1 = sqrt $ (x1-c1)**2 + (y1-c2)**2
  r2 = sqrt $ (x2-c1)**2 + (y2-c2)**2
  x1' = c1 + r1 * cos(theta1+psi)
  y1' = c2 + r1 * sin(theta1+psi)
  x2' = c1 + r2 * cos(theta2+psi)
  y2' = c2 + r2 * sin(theta2+psi)
  in distfunc p1 p2 x1' y1' x2' y2'
