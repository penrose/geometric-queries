module Gradients (
  -- two polygons
  bdixB,
  bdixAB,
  containB,
  containedB,
  containAB,
  containedAB,
  disjB,
  disjAB,
  inTangB,
  outTangB
) where

import PointsAndLines
import Polygons
import Linesearch
import Utils
import Debug.Trace

stepIntervalRel::Double
stepIntervalRel = 1

--[k1,k2,k3] = [10, 2, 1]

bsize = 50

---------- Below: encourage or discourage queries ----------

outTangB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double], Point)
outTangB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  polyA' = scalePGk c' polyA (1/bsize)
  polyB' = scalePGk c' polyB (1/bsize)
  c' = let ((x1,y1),(x2,y2)) = bbox polyB in ((x1+x2)/2, (y1+y2)/2)
  f cum = (energyInsideGGC polyA' polyB' c' cum) + 
    (energyInsideGGC' polyA' polyB' c' cum) +
    (mindsqGGC polyA' polyB' c' cum)
  g cum = add' (energyInsideGradGGC polyA' polyB' c' cum) $
    add' (energyInsideGradGGC' polyA' polyB' c' cum)
    (mindsqGradGGC polyA' polyB' c' cum)
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB' c' f g [0,0,0,1]
  in (scalePGk c' resPoly bsize, f1, g1, [0,0,t+r3,s*r4], c')

-- make B inside A and tangent to A

inTangB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double], Point)
inTangB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  polyA' = scalePGk c' polyA (1/bsize)
  polyB' = scalePGk c' polyB (1/bsize)
  c' = let ((x1,y1),(x2,y2)) = bbox polyB in ((x1+x2)/2, (y1+y2)/2)
  f cum = (energyOutsideGGC polyA' polyB' c' cum) + (mindsqGGC polyA' polyB' c' cum)
  g cum = add' (energyOutsideGradGGC polyA' polyB' c' cum)
    (mindsqGradGGC polyA' polyB' c' cum)
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB' c' f g [0,0,0,1]
  in (scalePGk c' resPoly bsize, f1, g1, [0,0,t+r3,s*r4], c')

-- make A, B disjoint

disjB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double], Point)
disjB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  polyA' = scalePGk c' polyA (1/bsize)
  polyB' = scalePGk c' polyB (1/bsize)
  c' = let ((x1,y1),(x2,y2)) = bbox polyB in ((x1+x2)/2, (y1+y2)/2)
  f cumulative = (energyInsideGGC polyA' polyB' c' cumulative) 
    + (energyInsideGGC' polyA' polyB' c' cumulative)
  g cumulative = add' (energyInsideGradGGC polyA' polyB' c' cumulative)
    (energyInsideGradGGC' polyA' polyB' c' cumulative)
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB' c' f g [0,0,0,1]
  in (scalePGk c' resPoly bsize, f1, g1, [0, 0, t+r3, s*r4], c')

disjAB :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] 
          -> (Polygon, Double, Double, [Double], Polygon)
disjAB polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  (polyBres, eB, gradB, cumB, cB') = disjB polyA polyB cB [c21,c22,c23,c24] weights
  (polyAres, eA, gradA, cumA, cA') = disjB polyB polyA cA [c11,c12,c13,c14] weights
  in (polyAres, eB+eA, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)

----- containment -----

-- make B contain A
containedB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double], Point)
containedB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  c' = let ((x1,y1),(x2,y2)) = bbox polyB in ((x1+x2)/2, (y1+y2)/2)
  polyA' = scalePGk c' polyA (1/bsize)
  polyB' = scalePGk c' polyB (1/bsize)
  f = energyOutsideGGC' polyA' polyB' c'
  g = energyOutsideGradGGC' polyA' polyB' c'
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB' c' f g [0, 0, 0, 1]
  in (scalePGk c' resPoly bsize, f1, g1, [0, 0, t+r3, s*r4], c')

containedAB :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] 
          -> (Polygon, Double, Double, [Double], Polygon)
containedAB polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  (polyBres, eB, gradB, cumB, cB') = containedB polyA polyB cB [c21,c22,c23,c24] weights
  (polyAres, eA, gradA, cumA, cA') = containB polyB polyA cA [c11,c12,c13,c14] weights
  in (polyAres, eB+eA, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)

-- make A contain B
containB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double], Point)
containB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  c' = let ((x1,y1),(x2,y2)) = bbox polyB in ((x1+x2)/2, (y1+y2)/2)
  polyA' = scalePGk c' polyA (1/bsize)
  polyB' = scalePGk c' polyB (1/bsize)
  f = energyOutsideGGC polyA' polyB' c'
  g = energyOutsideGradGGC polyA' polyB' c'
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB' c' f g [0, 0, 0, 1]
  in (scalePGk c' resPoly bsize, f1, g1, [0, 0, t+r3, s*r4], c')

containAB :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] 
          -> (Polygon, Double, Double, [Double], Polygon)
containAB polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  (polyBres, eB, gradB, cumB, cB') = containB polyA polyB cB [c21,c22,c23,c24] weights
  (polyAres, eA, gradA, cumA, cA') = containedB polyB polyA cA [c11,c12,c13,c14] weights
  in (polyAres, eB+eA, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)

----- boundary intersection -----

bdixAB :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] 
          -> (Polygon, Double, Double, [Double], Polygon)
bdixAB polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  (polyBres, eB, gradB, cumB) = bdixB polyA polyB cB [c21,c22,c23,c24] weights
  (polyAres, eA, gradA, cumA) = bdixB polyB polyA cA [c11,c12,c13,c14] weights
  in (polyAres, eB+eA, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)

bdixB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double])
bdixB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  c' = let ((x1,y1),(x2,y2)) = bbox polyB in ((x1+x2)/2, (y1+y2)/2)
  polyA' = scalePGk c' polyA (1/bsize)
  polyB' = scalePGk c' polyB (1/bsize)
  f = mindsqGGC polyA' polyB' c'
  g = mindsqGradGGC polyA' polyB' c'
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB' c' f g [0,0,0,1]
  in (scalePGk c' resPoly bsize, f1, g1, [0, 0, t+r3, s*r4])

---------- Helper for outputting to JS ----------

-- goal: everything this function sees is in warped space coordinates.
output polyA polyB c f g cumulative = trace ("Optimizing around center: "++(show c)) $ let
  [cumT1, cumT2, cumR, cumS] = cumulative
  gradRaw = g cumulative
  dir = neg' $ normalize' $ gradRaw--g cumulative 
  [m1', m2', r', s'] = valof' 4 $ linesearch f g dir cumulative
  delt = [m1', m2', r', s']
  appliedTRS = transformG polyB c [cumT1+m1',cumT2+m2',cumR+r',cumS+s']
  g0 = mag' $ g $ add' cumulative delt
  f0 = f $ add' cumulative delt
  in (appliedTRS, if f (add' cumulative delt) < epsilon then 0 
    else f0, g0, add' cumulative delt)

---------- Below: 3 energies (and their gradients) ----------

mindsqGGC :: Polygon -> Polygon -> Point -> [Double] -> Double
mindsqGGC polyA polyB c [m1, m2, r, s] = let
  appliedTRS = transformG polyB c [m1,m2,r,s]
  in (unsignedDistGG polyA appliedTRS)**2

mindsqGradGGC :: Polygon -> Polygon -> Point -> [Double] -> [Double]
mindsqGradGGC polyA polyB (cx,cy) [mx, my, t, s] = let
  appliedTRS = transformG polyB (cx,cy) [mx,my,t,s]
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

energyInsideGradGGC' :: Polygon -> Polygon -> Point -> [Double] -> [Double]
energyInsideGradGGC' polyA polyB c cumulative = 
  dsqIntegralGradGGC' polyA polyB c cumulative False
  
energyInsideGGC' :: Polygon -> Polygon -> Point -> [Double] -> Double
energyInsideGGC' polyA polyB c cumulative = 
  dsqIntegralGGC' polyA polyB c cumulative False

---------- Helpers for calculating energies (and gradients) ----------

dsqIntegralGGC :: Polygon -> Polygon -> Point -> [Double] -> Bool -> Double
dsqIntegralGGC polyA polyB c [mx,my,t,s] fromOutside = let
  stepInterval = stepIntervalRel / bsize
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ getSegments polyB
  transformedSamples = transformG samples c [mx,my,t,s]
  closestPs = map (closestPointGP polyA) transformedSamples
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
  stepInterval = stepIntervalRel / bsize
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

dsqIntegralGGC' :: Polygon -> Polygon -> Point -> [Double] -> Bool -> Double
dsqIntegralGGC' polyA polyB c [mx,my,t,s] fromOutside = let
  stepInterval = stepIntervalRel / bsize
  -- sample pts on A
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ getSegments polyA
  appliedTRS = transformG polyB c [mx,my,t,s]
  -- closest points on B cor. to samples on A
  closestPs = map (closestPointGP appliedTRS) samples
  zp = zip samples closestPs
  -- sum of distsq bt. sample pts on A, and B
  resFromOut = foldl (\a b->a+b) 0 $ map (\(a,b)->
    if outsidedness appliedTRS a < 0 then 0 else distsq a b) zp
  resFromIn = foldl (\a b->a+b) 0 $ map (\(a,b)->
    if outsidedness appliedTRS a > 0 then 0 else distsq a b) zp
  res = (if fromOutside then resFromOut else resFromIn) * stepInterval
  in res

dsqIntegralGradGGC' :: Polygon -> Polygon -> Point -> [Double] -> Bool -> [Double]
dsqIntegralGradGGC' polyA polyB c [mx,my,t,s] fromOutside = let
  stepInterval = stepIntervalRel / bsize
  -- sample pts on A
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ getSegments polyA
  transformedB = transformG polyB c [mx,my,t,s]
  -- below: closest pts on B that map to sample pts on A
  closestPs = map (closestPointGP transformedB) samples
  cpOrig = map (\p -> let
    revS = scalePPk c p (1/s)
    revSR = rotateAroundPPa c revS (-t)
    in movebyPm revSR (-mx,-my)) closestPs
  zp = zip samples cpOrig
  resFromOut = mult' stepInterval
    $ foldl add' [0,0,0,0] 
    $ map (\(a,b)-> if outsidedness transformedB a < 0 then [0,0,0,0] 
      else reduceDistCPbPa c b a [mx,my,t,s]
    ) zp
  resFromIn = mult' stepInterval
    $ foldl add' [0,0,0,0] 
    $ map (\(a,b)-> if outsidedness transformedB a > 0 then [0,0,0,0] 
      else reduceDistCPbPa c b a [mx,my,t,s]
    ) zp
  in if fromOutside then resFromOut else resFromIn

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
