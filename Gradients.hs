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

bsize = 100


---------- Below: encourage or discourage queries ----------

outTangB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double])
outTangB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  polyA' = scalePGk (0,0) polyA (1/bsize)
  polyB' = scalePGk (0,0) polyB (1/bsize)
  (cx,cy) = if (mx,my)==(0,0) then polyB'!!0 else (mx,my)
  (offx,offy) = polyB'!!0
  polyB0 = movebyGm polyB' (-offx,-offy)
  f cum = (energyInsideGGC polyA' polyB0 cum) + 
    (energyInsideGGC' polyA' polyB0 cum) +
    (mindsqGGC polyA' polyB0 cum)
  g cum = add' (energyInsideGradGGC polyA' polyB0 cum) $
    add' (energyInsideGradGGC' polyA' polyB0 cum)
    (mindsqGradGGC polyA' polyB0 cum)
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB0 f g [cx,cy,t,s]
  in (scalePGk (0,0) resPoly bsize, f1, g1, [r1,r2,r3,r4])

-- make B inside A and tangent to A

inTangB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double])
inTangB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  polyA' = scalePGk (0,0) polyA (1/bsize)
  polyB' = scalePGk (0,0) polyB (1/bsize)
  (cx,cy) = if (mx,my)==(0,0) then polyB'!!0 else (mx,my)
  (offx,offy) = polyB'!!0
  polyB0 = movebyGm polyB' (-offx,-offy)
  f cum = (energyOutsideGGC polyA' polyB0 cum) + (mindsqGGC polyA' polyB0 cum)
  g cum = add' (energyOutsideGradGGC polyA' polyB0 cum)
    (mindsqGradGGC polyA' polyB0 cum)
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB0 f g [cx,cy,t,s]
  in (scalePGk (0,0) resPoly bsize, f1, g1, [r1,r2,r3,r4])

-- make A, B disjoint

disjB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double])
disjB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  polyA' = scalePGk (0,0) polyA (1/bsize)
  polyB' = scalePGk (0,0) polyB (1/bsize)
  (cx,cy) = if (mx,my)==(0,0) then polyB'!!0 else (mx,my)
  (offx,offy) = polyB'!!0
  polyB0 = movebyGm polyB' (-offx,-offy)
  f cumulative = (energyInsideGGC polyA' polyB0 cumulative) 
    + (energyInsideGGC' polyA' polyB0 cumulative)
  g cumulative = add' (energyInsideGradGGC polyA' polyB0 cumulative)
    (energyInsideGradGGC' polyA' polyB0 cumulative)
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB0 f g [cx,cy,t,s]
  in (scalePGk (0,0) resPoly bsize, f1, g1, [r1,r2,r3,r4])

disjAB :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] 
          -> (Polygon, Double, Double, [Double], Polygon)
disjAB polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  polyA' = scalePGk (0,0) polyA (1/bsize)
  polyB' = scalePGk (0,0) polyB (1/bsize)
  (cxA,cyA) = (c11,c12)
  (cxB,cyB) = (c21,c22)
  (offxA, offyA) = polyA'!!0
  (offxB, offyB) = polyB'!!0
  polyA0 = movebyGm polyA' (-offxA, -offyA)
  polyB0 = movebyGm polyB' (-offxB, -offyB)
  polyAnow_s = transformG polyA0 [c11,c12,c13,c14]
  polyBnow_s = transformG polyB0 [c21,c22,c23,c24]
  polyAnow = if (c11,c12)==(0,0) then polyA else scalePGk (0,0) polyAnow_s bsize
  polyBnow = if (c21,c22)==(0,0) then polyB else scalePGk (0,0) polyBnow_s bsize
  (polyBres, eB, gradB, cumB) = disjB polyAnow polyB cB [c21,c22,c23,c24] weights
  (polyAres, eA, gradA, cumA) = disjB polyBnow polyA cA [c11,c12,c13,c14] weights
  in (polyAres, eB+eA, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)

----- containment -----

-- make B contain A
containedB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double])
containedB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  polyA' = scalePGk (0,0) polyA (1/bsize)
  polyB' = scalePGk (0,0) polyB (1/bsize)
  (cx,cy) = if (mx,my)==(0,0) then polyB'!!0 else (mx,my)
  (offx,offy) = polyB'!!0
  polyB0 = movebyGm polyB' (-offx,-offy)
  f = energyOutsideGGC' polyA' polyB0
  g = energyOutsideGradGGC' polyA' polyB0
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB0 f g [cx,cy,t,s]
  in (scalePGk (0,0) resPoly bsize, f1, g1, [r1,r2,r3,r4])


containedAB :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] 
          -> (Polygon, Double, Double, [Double], Polygon)
containedAB polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  polyA' = scalePGk (0,0) polyA (1/bsize)
  polyB' = scalePGk (0,0) polyB (1/bsize)
  (cxA,cyA) = (c11,c12)
  (cxB,cyB) = (c21,c22)
  (offxA, offyA) = polyA'!!0
  (offxB, offyB) = polyB'!!0
  polyA0 = movebyGm polyA' (-offxA, -offyA)
  polyB0 = movebyGm polyB' (-offxB, -offyB)
  polyAnow_s = transformG polyA0 [c11,c12,c13,c14]
  polyBnow_s = transformG polyB0 [c21,c22,c23,c24]
  polyAnow = if (c11,c12)==(0,0) then polyA else scalePGk (0,0) polyAnow_s bsize
  polyBnow = if (c21,c22)==(0,0) then polyB else scalePGk (0,0) polyBnow_s bsize
  (polyBres, eB, gradB, cumB) = containedB polyAnow polyB cB [c21,c22,c23,c24] weights
  (polyAres, eA, gradA, cumA) = containB polyBnow polyA cA [c11,c12,c13,c14] weights
  in (polyAres, eB+eA, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)


-- make A contain B
containB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double])
containB polyA polyB c [mx,my,t,s] [k1, k2, k3] = let
  polyA' = scalePGk (0,0) polyA (1/bsize)
  polyB' = scalePGk (0,0) polyB (1/bsize)
  (cx,cy) = if (mx,my)==(0,0) then polyB'!!0 else (mx,my)
  (offx,offy) = polyB'!!0
  polyB0 = movebyGm polyB' (-offx,-offy)
  f = energyOutsideGGC polyA' polyB0
  g = energyOutsideGradGGC polyA' polyB0
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB0 f g [cx,cy,t,s]
  in (scalePGk (0,0) resPoly bsize, f1, g1, [r1,r2,r3,r4])


containAB :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] 
          -> (Polygon, Double, Double, [Double], Polygon)
containAB polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  polyA' = scalePGk (0,0) polyA (1/bsize)
  polyB' = scalePGk (0,0) polyB (1/bsize)
  (cxA,cyA) = (c11,c12)
  (cxB,cyB) = (c21,c22)
  (offxA, offyA) = polyA'!!0
  (offxB, offyB) = polyB'!!0
  polyA0 = movebyGm polyA' (-offxA, -offyA)
  polyB0 = movebyGm polyB' (-offxB, -offyB)
  polyAnow_s = transformG polyA0 [c11,c12,c13,c14]
  polyBnow_s = transformG polyB0 [c21,c22,c23,c24]
  polyAnow = if (c11,c12)==(0,0) then polyA else scalePGk (0,0) polyAnow_s bsize
  polyBnow = if (c21,c22)==(0,0) then polyB else scalePGk (0,0) polyBnow_s bsize
  (polyBres, eB, gradB, cumB) = containB polyAnow polyB cB [c21,c22,c23,c24] weights
  (polyAres, eA, gradA, cumA) = containedB polyBnow polyA cA [c11,c12,c13,c14] weights
  in (polyAres, eB+eA, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)


----- boundary intersection -----

bdixAB :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] 
          -> (Polygon, Double, Double, [Double], Polygon)
bdixAB polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  polyA' = scalePGk (0,0) polyA (1/bsize)
  polyB' = scalePGk (0,0) polyB (1/bsize)
  (cxA,cyA) = (c11,c12)
  (cxB,cyB) = (c21,c22)
  (offxA, offyA) = polyA'!!0
  (offxB, offyB) = polyB'!!0
  polyA0 = movebyGm polyA' (-offxA, -offyA)
  polyB0 = movebyGm polyB' (-offxB, -offyB)
  polyAnow_s = transformG polyA0 [c11,c12,c13,c14]
  polyBnow_s = transformG polyB0 [c21,c22,c23,c24]
  polyAnow = if (c11,c12)==(0,0) then polyA else scalePGk (0,0) polyAnow_s bsize
  polyBnow = if (c21,c22)==(0,0) then polyB else scalePGk (0,0) polyBnow_s bsize
  (polyBres, eB, gradB, cumB) = bdixB polyAnow polyB cB [c21,c22,c23,c24] weights
  (polyAres, eA, gradA, cumA) = bdixB polyBnow polyA cA [c11,c12,c13,c14] weights
  in (polyAres, eB+eA, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)

bdixB :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> 
  (Polygon, Double, Double, [Double])
bdixB polyA polyB dsafdsa [mx,my,t,s] [k1, k2, k3] = let
  polyA' = scalePGk (0,0) polyA (1/bsize)
  polyB' = scalePGk (0,0) polyB (1/bsize)
  (cx,cy) = if (mx,my)==(0,0) then polyB'!!0 else (mx,my)
  (offx, offy) = polyB'!!0
  polyB0 = movebyGm polyB' (-offx,-offy)
  f = mindsqGGC polyA' polyB0
  g = mindsqGradGGC polyA' polyB0
  (resPoly, f1, g1, [r1,r2,r3,r4]) = output polyA' polyB0 f g [cx,cy,t,s]
  in (scalePGk (0,0) resPoly bsize, f1, g1, [r1, r2, r3, r4])

---------- Helper for outputting to JS ----------

--TODO in linesearch: quit immediately when f1 = 0

-- also polyB should be the ORIGINAL B, centered at origin.
-- goal: everything this function sees is in warped space coordinates.
output polyA polyB f g cumulative = let
  [cumT1, cumT2, cumR, cumS] = cumulative
  gradRaw = g cumulative
  dir = neg' $ normalize' $ gradRaw--g cumulative 
  [m1', m2', r', s'] = valof' 4 $ linesearch f g dir cumulative
  delt = [m1', m2', r', s']
  appliedTRS = transformG polyB [cumT1+m1',cumT2+m2',cumR+r',cumS+s']
  g0 = mag' $ g $ add' cumulative delt
  f0 = f $ add' cumulative delt
  in (appliedTRS, if f (add' cumulative delt) < epsilon then 0 
    else f0, g0, add' cumulative delt)

---------- Below: 3 energies (and their gradients) ----------

-- input B0
mindsqGGC :: Polygon -> Polygon -> [Double] -> Double
mindsqGGC polyA polyB [m1, m2, r, s] = let
  appliedTRS = transformG polyB [m1,m2,r,s]
  in (unsignedDistGG polyA appliedTRS)**2

-- input B0
mindsqGradGGC :: Polygon -> Polygon -> [Double] -> [Double]
mindsqGradGGC polyA polyB [mx, my, t, s] = let
  appliedTRS = transformG polyB [mx,my,t,s]
  ((x1',y1), (x2',y2)) = shortestSegmentGG polyA appliedTRS
  fromA = (dist (x1',y1) $ closestPointGP polyA (x1',y1)) < epsilon
  ((v1,v2), (x1'',x2'')) = if fromA then ((x1',y1),(x2',y2)) else ((x2',y2),(x1',y1))
  revT = movebyPm (x1'',x2'') (-mx,-my)
  revTS = scalePPk (0,0) revT (1/s)
  x = rotateAroundPPa (0,0) revTS (-t)
  in reduceDistCPbPa x (v1,v2) [mx,my,t,s]

energyOutsideGradGGC :: Polygon -> Polygon -> [Double] -> [Double]
energyOutsideGradGGC polyA polyB cumulative = 
  dsqIntegralGradGGC polyA polyB cumulative True
  
energyOutsideGGC :: Polygon -> Polygon -> [Double] -> Double
energyOutsideGGC polyA polyB cumulative = 
  dsqIntegralGGC polyA polyB cumulative True

energyOutsideGradGGC' :: Polygon -> Polygon -> [Double] -> [Double]
energyOutsideGradGGC' polyA polyB cumulative = 
  dsqIntegralGradGGC' polyA polyB cumulative True
  
energyOutsideGGC' :: Polygon -> Polygon -> [Double] -> Double
energyOutsideGGC' polyA polyB cumulative = 
  dsqIntegralGGC' polyA polyB cumulative True

energyInsideGradGGC :: Polygon -> Polygon -> [Double] -> [Double]
energyInsideGradGGC polyA polyB cumulative = 
  dsqIntegralGradGGC polyA polyB cumulative False
  
energyInsideGGC :: Polygon -> Polygon -> [Double] -> Double
energyInsideGGC polyA polyB cumulative = 
  dsqIntegralGGC polyA polyB cumulative False

energyInsideGradGGC' :: Polygon -> Polygon -> [Double] -> [Double]
energyInsideGradGGC' polyA polyB cumulative = 
  dsqIntegralGradGGC' polyA polyB cumulative False
  
energyInsideGGC' :: Polygon -> Polygon -> [Double] -> Double
energyInsideGGC' polyA polyB cumulative = 
  dsqIntegralGGC' polyA polyB cumulative False

---------- Helpers for calculating energies (and gradients) ----------

dsqIntegralGGC :: Polygon -> Polygon -> [Double] -> Bool -> Double
dsqIntegralGGC polyA polyB [mx,my,t,s] fromOutside = let
  stepInterval = stepIntervalRel / bsize
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ getSegments polyB
  transformedSamples = transformG samples [mx,my,t,s]
  closestPs = map (closestPointGP polyA) transformedSamples
  sampleSumOut = foldl (\a b->a+b) 0 $ map (\p->
    if outsidedness polyA p < 0 then 0 else distsq p $ closestPointGP polyA p
    ) transformedSamples
  sampleSumIn = foldl (\a b->a+b) 0 $ map (\p->
    if outsidedness polyA p > 0 then 0 else distsq p $ closestPointGP polyA p
    ) transformedSamples
  res = (if fromOutside then sampleSumOut else sampleSumIn) * stepInterval
  in res

dsqIntegralGradGGC :: Polygon -> Polygon -> [Double] -> Bool -> [Double]
dsqIntegralGradGGC polyA polyB [mx,my,t,s] fromOutside = let
  stepInterval = stepIntervalRel / bsize
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ getSegments polyB
  transformedSamples = transformG samples [mx,my,t,s]
  closestPs = map (closestPointGP polyA) transformedSamples
  zp = zip closestPs samples
  resFromOut = mult' stepInterval --mult' ((fromIntegral $ length samples)/stepInterval) $
    $ foldl add' [0,0,0,0] 
    $ map (\(a,b)-> let b' = transformP b [mx,my,t,s] in
      if outsidedness polyA b' < 0 then [0,0,0,0] 
      else reduceDistCPbPa b a [mx,my,t,s]
    ) zp
  resFromIn = mult' stepInterval --mult' ((fromIntegral $ length samples)/stepInterval) $
    $ foldl add' [0,0,0,0] 
    $ map (\(a,b)-> let b' = transformP b [mx,my,t,s] in
      if outsidedness polyA b' > 0 then [0,0,0,0] 
      else reduceDistCPbPa b a [mx,my,t,s]
    ) zp
  in if fromOutside then resFromOut else resFromIn


dsqIntegralGGC' :: Polygon -> Polygon -> [Double] -> Bool -> Double
dsqIntegralGGC' polyA polyB [mx,my,t,s] fromOutside = let
  stepInterval = stepIntervalRel / bsize
  -- sample pts on A
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ getSegments polyA
  transformedB = transformG polyB [mx,my,t,s]
  -- closest points on B cor. to samples on A
  closestPs = map (closestPointGP transformedB) samples
  zp = zip samples closestPs
  -- sum of distsq bt. sample pts on A, and B
  resFromOut = foldl (\a b->a+b) 0 $ map (\(a,b)->
    if outsidedness transformedB a < 0 then 0 else distsq a b) zp
  resFromIn = foldl (\a b->a+b) 0 $ map (\(a,b)->
    if outsidedness transformedB a > 0 then 0 else distsq a b) zp
  res = (if fromOutside then resFromOut else resFromIn) * stepInterval
  in res

dsqIntegralGradGGC' :: Polygon -> Polygon -> [Double] -> Bool -> [Double]
dsqIntegralGradGGC' polyA polyB [mx,my,t,s] fromOutside = let
  stepInterval = stepIntervalRel / bsize
  -- sample pts on A
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ getSegments polyA
  transformedB = transformG polyB [mx,my,t,s]
  -- below: closest pts on B that map to sample pts on A
  closestPs = map (closestPointGP transformedB) samples
  cpOrig = map (\p -> let
    revT = movebyPm p (-mx,-my)
    revTS = scalePPk (0,0) revT (1/s)
    in rotateAroundPPa (0,0) revTS (-t)) closestPs
  zp = zip samples cpOrig
  resFromOut = mult' stepInterval
    $ foldl add' [0,0,0,0] 
    $ map (\(a,b)-> if outsidedness transformedB a < 0 then [0,0,0,0] 
      else reduceDistCPbPa b a [mx,my,t,s]
    ) zp
  resFromIn = mult' stepInterval
    $ foldl add' [0,0,0,0] 
    $ map (\(a,b)-> if outsidedness transformedB a > 0 then [0,0,0,0] 
      else reduceDistCPbPa b a [mx,my,t,s]
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
reduceDistCPbPa :: Point -> Point -> [Double] -> [Double]
reduceDistCPbPa (x1,x2) (v1,v2) [mx, my, t, s] = let
  -- ok ready...
  cost = cos t
  sint = sin t
  (u1, u2) = transformP (x1,x2) [mx,my,t,s]
  u1_v1 = u1 - v1
  u2_v2 = u2 - v2
  dmx = 2*u1_v1
  dmy = 2*u2_v2
  dt = 2*u1_v1 * ( -s*sint*x1 - s*cost*x2)
    + 2*u2_v2 * (s*cost*x1 - s*sint*x2)
  ds = 2*u1_v1 * (cost*x1 - sint*x2)
    + 2*u2_v2 * (sint*x1 + cost*x2)
  in [dmx, dmy, dt, ds]
