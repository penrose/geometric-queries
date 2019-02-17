module Gradients (
  -- two polygons
  bdixB,
  bdixAB,
  containB,
  containedB,
  disjB,
  inTangB,
  outTangB
) where

import PointsAndLines
import Polygons
import Linesearch
import Utils
import Debug.Trace

stepInterval::Double
stepInterval = 2


---------- Below: encourage or discourage queries ----------

outTangB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> (Polygon, Double, Double, [Double])
outTangB polyA polyB c cumulative [k1, k2, k3] = let
  f cum = (energyInsideGGC polyA polyB c cum) + (mindsqGGC polyA polyB c cum)
  g cum = let 
    [m1',m2',r',s'] = add' (energyInsideGradGGC polyA polyB c cum) 
                           (mindsqGradGGC polyA polyB c cum)
    in [k1*m1', k1*m2', k2*r', k3*s']
  in output polyA polyB c f g cumulative

inTangB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> (Polygon, Double, Double, [Double])
inTangB polyA polyB c cumulative [k1, k2, k3] = let
  f cum = (energyOutsideGGC polyA polyB c cum) + (mindsqGGC polyA polyB c cum)
  g cum = let 
    [m1',m2',r',s'] = add' (energyOutsideGradGGC polyA polyB c cum) 
                           (mindsqGradGGC polyA polyB c cum)
    in [k1*m1', k1*m2', k2*r', k3*s']
  in output polyA polyB c f g cumulative

disjB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> (Polygon, Double, Double, [Double])
disjB polyA polyB c cumulative [k1, k2, k3] = let
  f = energyInsideGGC polyA polyB c
  g cumulative = let 
    [m1',m2',r',s'] = energyInsideGradGGC polyA polyB c cumulative
    in [k1*m1', k1*m2', k2*r', k3*s']
  in output polyA polyB c f g cumulative

{-
containAB :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] 
          -> (Polygon, Double, [Double], Polygon)
containAB polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  polyA' = scalePGk cA (rotateAroundPGa cA (movebyGm polyA (c11,c12)) c13) c14
  polyB' = scalePGk cB (rotateAroundPGa cB (movebyGm polyB (c21,c22)) c23) c24
  (polyBres, gradB, cumB) = bdixB polyA' polyB cB [c21,c22,c23,c24] weights
  (polyAres, gradA, cumA) = containB polyB' polyA cA [c11,c12,c13,c14] weights
  in (polyAres, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)
-}

containedB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> (Polygon, Double, Double, [Double])
containedB polyA polyB c cumulative [k1, k2, k3] = let
  f = energyOutsideGGC' polyA polyB c
  g cumulative = let 
    [m1',m2',r',s'] = neg' $ energyOutsideGradGGC' polyA polyB c cumulative
    in [k1*m1', k1*m2', k2*r', k3*s']
  in output polyA polyB c f g cumulative

-- this version of encouraging containment: uses integration of distsq along edge as energy
containB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> (Polygon, Double, Double, [Double])
containB polyA polyB c cumulative [k1, k2, k3] = let
  f = energyOutsideGGC polyA polyB c
  g cumulative = let 
    [m1',m2',r',s'] = energyOutsideGradGGC polyA polyB c cumulative
    in [k1*m1', k1*m2', k2*r', k3*s']
  in output polyA polyB c f g cumulative

bdixAB :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] 
          -> (Polygon, Double, [Double], Polygon)
bdixAB polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  polyA' = scalePGk cA (rotateAroundPGa cA (movebyGm polyA (c11,c12)) c13) c14
  polyB' = scalePGk cB (rotateAroundPGa cB (movebyGm polyB (c21,c22)) c23) c24
  (polyBres, eB, gradB, cumB) = bdixB polyA' polyB cB [c21,c22,c23,c24] weights
  (polyAres, eA, gradA, cumA) = bdixB polyB' polyA cA [c11,c12,c13,c14] weights
  in (polyAres, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)

bdixB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> (Polygon, Double, Double, [Double])
bdixB polyA polyB c cumulative [k1, k2, k3] = let
  f = mindsqGGC polyA polyB c
  g [m1,m2,r,s] = res where
    [m1',m2',r',s'] = mindsqGradGGC polyA polyB c [m1,m2,r,s]
    res = [k1*m1', k1*m2', k2*r', k3*s']
  in output polyA polyB c f g cumulative

---------- Helper for outputting to JS ----------

output polyA polyB c f g cumulative = let
  [cumT1, cumT2, cumR, cumS] = cumulative
  gradRaw = g cumulative
  dir = neg' $ normalize' $ gradRaw--g cumulative 
  [m1', m2', r', s'] = valof' 4 $ linesearch f g dir cumulative
  delt = [m1', m2', r', s']
  appliedTRS = transformG polyB c [cumT1+m1',cumT2+m2',cumR+r',cumS+s']
  g0 = mag' $ g $ add' cumulative delt
  f0 = f $ add' cumulative delt
  in (appliedTRS, if f (add' cumulative delt) < epsilon then 0 else f0, g0, add' cumulative delt)

---------- Below: 3 energies (and gradients) ----------

mindsqGGC :: Polygon -> Polygon -> Point -> [Double] -> Double
mindsqGGC polyA polyB c [m1, m2, r, s] = let
  appliedT = movebyGm polyB (m1,m2)
  appliedTR = rotateAroundPGa c appliedT r
  appliedTRS = scalePGk c appliedTR s
  in (unsignedDistGG polyA appliedTRS)**2

mindsqGradGGC :: Polygon -> Polygon -> Point -> [Double] -> [Double]
mindsqGradGGC polyA polyB (cx,cy) [mx, my, t, s] = let
  appliedT = movebyGm polyB (mx,my)
  appliedTR = rotateAroundPGa (cx,cy) appliedT t
  appliedTRS = scalePGk (cx,cy) appliedTR s
  ((x1',y1), (x2',y2)) = shortestSegmentGG polyA appliedTRS
  fromA = (dist (x1',y1) $ closestPointGP polyA (x1',y1)) < epsilon
  ((v1,v2), (x1'',x2'')) = if fromA then ((x1',y1),(x2',y2)) else ((x2',y2),(x1',y1))
  revS = scalePPk (cx,cy) (x1'',x2'') (1/s)
  revSR = rotateAroundPPa (cx,cy) revS (-t)
  (x1,x2) = movebyPm revSR (-mx,-my)
  in reduceDistCPbPa (cx,cy) (x1,x2) (v1,v2) [mx,my,t,s] 

energyOutsideGradGGC :: Polygon -> Polygon -> Point -> [Double] -> [Double]
energyOutsideGradGGC polyA polyB c cumulative = 
  dsqIntegralGradGGC polyA polyB c cumulative True
  
energyOutsideGGC :: Polygon -> Polygon -> Point -> [Double] -> Double
energyOutsideGGC polyA polyB c cumulative = 
  dsqIntegralGGC polyA polyB c cumulative True

energyOutsideGradGGC' :: Polygon -> Polygon -> Point -> [Double] -> [Double]
energyOutsideGradGGC' polyA polyB c cumulative = 
  dsqIntegralGradGGC' polyA polyB c cumulative True
  
energyOutsideGGC' :: Polygon -> Polygon -> Point -> [Double] -> Double
energyOutsideGGC' polyA polyB c cumulative = 
  dsqIntegralGGC' polyA polyB c cumulative True

energyInsideGradGGC :: Polygon -> Polygon -> Point -> [Double] -> [Double]
energyInsideGradGGC polyA polyB c cumulative = 
  dsqIntegralGradGGC polyA polyB c cumulative False
  
energyInsideGGC :: Polygon -> Polygon -> Point -> [Double] -> Double
energyInsideGGC polyA polyB c cumulative = 
  dsqIntegralGGC polyA polyB c cumulative False

---------- Helpers for calculating energies (and gradients) ----------

dsqIntegralGGC :: Polygon -> Polygon -> Point -> [Double] -> Bool -> Double
dsqIntegralGGC polyA polyB c [mx,my,t,s] fromOutside = let
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ getSegments polyB
  transformedSamples = transformG samples c [mx,my,t,s]
  sampleSumOut = foldl (\a b->a+b) 0 $ map (\p->
    if outsidedness polyA p < 0 then 0 else distsq p $ closestPointGP polyA p
    ) transformedSamples
  sampleSumIn = foldl (\a b->a+b) 0 $ map (\p->
    if outsidedness polyA p > 0 then 0 else distsq p $ closestPointGP polyA p
    ) transformedSamples
  res = (if fromOutside then sampleSumOut else sampleSumIn) * stepInterval
  in res

dsqIntegralGradGGC :: Polygon -> Polygon -> Point -> [Double] -> Bool -> [Double]
dsqIntegralGradGGC polyA polyB c [mx,my,t,s] fromOutside = let
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ getSegments polyB
  transformedSamples = transformG samples c [mx,my,t,s]
  closestPs = map (closestPointGP polyA) transformedSamples
  samplesOrig = map (\p -> let
    revS = scalePPk c p (1/s)
    revSR = rotateAroundPPa c revS (-t)
    in movebyPm revSR (-mx,-my)) samples
  zp = zip closestPs samples--samplesOrig
  resFromOut = mult' stepInterval --mult' ((fromIntegral $ length samples)/stepInterval) $
    $ foldl add' [0,0,0,0] 
    $ map (\(a,b)-> let b' = transformP b c [mx,my,t,s] in
      if outsidedness polyA b' < 0 then [0,0,0,0] 
      else reduceDistCPbPa c b a [mx,my,t,s]
    ) zp
  resFromIn = mult' stepInterval --mult' ((fromIntegral $ length samples)/stepInterval) $
    $ foldl add' [0,0,0,0] 
    $ map (\(a,b)-> let b' = transformP b c [mx,my,t,s] in
      if outsidedness polyA b' > 0 then [0,0,0,0] 
      else reduceDistCPbPa c b a [mx,my,t,s]
    ) zp
  in if fromOutside then resFromOut else resFromIn

-- the alternative version: cuts A instead of B, used for making B contain A
dsqIntegralGGC' :: Polygon -> Polygon -> Point -> [Double] -> Bool -> Double
dsqIntegralGGC' polyA polyB c [mx,my,t,s] fromOutside = let
  appliedTRS = transformG polyB c [mx,my,t,s]
  (inside, outside) = cookiePoly appliedTRS polyA
  -- sample pts on A
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ 
    if fromOutside then outside else inside
  -- sum of distsq bt. sample pts on A, and B
  sampleSum = foldl (\a b->a+b) 0 $ map (\p->distsq p $ closestPointGP polyB p) samples
  res = sampleSum * stepInterval
  in res

dsqIntegralGradGGC' :: Polygon -> Polygon -> Point -> [Double] -> Bool -> [Double]
dsqIntegralGradGGC' polyA polyB c [mx,my,t,s] fromOutside = let
  appliedTRS = transformG polyB c [mx,my,t,s]
  (inside, outside) = cookiePoly appliedTRS polyA
  -- sample pts on A
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ 
    if fromOutside then outside else inside
  -- corresponding pts on B
  closestPs = map (closestPointGP polyB) samples
  -- corresponding original pts on B
  cpOrig = map (\p -> let
    revS = scalePPk c p (1/s)
    revSR = rotateAroundPPa c revS (-t)
    in movebyPm revSR (-mx,-my)) closestPs
  zp = zip samples cpOrig
  res = mult' ((fromIntegral $ length cpOrig)/stepInterval) 
    $ foldl add' [0,0,0,0] 
    $ map (\(a,b)->reduceDistCPbPa c a b [mx,my,t,s]) zp
  in res

-- returns a list of sample points along segment, each separated by stepsize pixels
sampleSeg :: Double -> LineSeg -> [Point]
sampleSeg stepsize (x,y) = let
  len = dist x y
  samplets = [0,(stepsize/len)..1]
  in map (fromT (x,y)) samplets
  
-- input: polyA, c, pt on A, and pt on B that: 
-- we know after the cumulative transformation it's closest to Pa.
-- returns the amt of transformation grad for pt on B.
reduceDistCPbPa :: Point -> Point -> Point -> [Double] -> [Double]
reduceDistCPbPa (cx,cy) (x1,x2) (v1,v2) [mx, my, t, s] = let
  -- ok ready...
  cost = cos t
  sint = sin t
  mx_cx = mx - cx
  my_cy = my - cy
  u1_v1 = s * cost*x1 - s * sint*x2 + s*cost*mx_cx - s*sint*my_cy + cx - v1
  u2_v2 = s * sint*x1 + s * cost*x2 + s*sint*mx_cx + s*cost*my_cy + cy - v2
  dmx = 2*u1_v1 * s*cost + 2*u2_v2 * s*sint
  dmy = -2*u1_v1 * s*sint + 2*u2_v2 * s*cost
  dt = 2*u1_v1 * ( -s*sint*x1 - s*cost*x2
    - s*sint*mx_cx - s*cost*my_cy)
    + 2*u2_v2 * (s*cost*x1 - s*sint*x2
    + s*cost*mx_cx - s*sint*my_cy)
  ds = 2*u1_v1 * (cost*x1 - sint*x2
    + cost*mx_cx - sint*my_cy)
    + 2*u2_v2 * (sint*x1 + cost*x2
    + sint*mx_cx + cost*my_cy)
  in [dmx, dmy, dt, ds]
