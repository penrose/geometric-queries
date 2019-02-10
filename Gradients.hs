module Gradients (
  -- two polygons
  combGGCout,
  combGCGCout,
  containGGCout
) where

import PointsAndLines
import Polygons
import Linesearch
import Utils
import Debug.Trace

stepInterval::Double
stepInterval = 4

---------- two polygons ------------

combGGC :: Polygon -> Polygon -> Point -> [Double] -> [Double]
combGGC polyA polyB (cx,cy) [mx, my, t, s] = let
  appliedT = movebyGm polyB (mx,my) --scalePGk c polyB s
  appliedTR = rotateAroundPGa (cx,cy) appliedT t
  appliedTRS = scalePGk (cx,cy) appliedTR s
  ((x1',y1), (x2',y2)) = shortestSegmentGG polyA appliedTRS
  fromA = (dist (x1',y1) $ closestPointGP polyA (x1',y1)) < epsilon
  ((v1,v2), (x1'',x2'')) = if fromA then ((x1',y1),(x2',y2)) else ((x2',y2),(x1',y1))
  revS = scalePPk (cx,cy) (x1'',x2'') (1/s)
  revSR = rotateAroundPPa (cx,cy) revS (-t)
  (x1,x2) = movebyPm revSR (-mx,-my)
  in reduceDistGCPbPa polyA (cx,cy) (x1,x2) (v1,v2) [mx,my,t,s]  

combGCGCout :: Polygon -> Point -> Polygon -> Point -> [Double] -> [Double] -> (Polygon, Double, [Double], Polygon)
combGCGCout polyA cA polyB cB [c11,c12,c13,c14, c21,c22,c23,c24] weights = let
  polyA' = scalePGk cA (rotateAroundPGa cA (movebyGm polyA (c11,c12)) c13) c14
  polyB' = scalePGk cB (rotateAroundPGa cB (movebyGm polyB (c21,c22)) c23) c24
  (polyBres, gradB, cumB, _) = combGGCout polyA' polyB cB [c21,c22,c23,c24] weights
  (polyAres, gradA, cumA, _) = combGGCout polyB' polyA cA [c11,c12,c13,c14] weights
  in trace ((show gradA)++" "++(show gradB)) $ (polyAres, sqrt $ gradA**2 + gradB**2, cumA++cumB, polyBres)

combGGCout :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> (Polygon, Double, [Double], Point)
combGGCout polyA polyB c cumulative [k1, k2, k3] = let
  [cumT1, cumT2, cumR, cumS] = cumulative
  -- energy: dist sqr after all transformations applied (trans -> rot -> scale)
  -- defining energy to be dist squared, but use gradient of distsq + 1/s (to disallow negative scale) ?
  f [m1,m2,r,s] = let 
    appliedT = movebyGm polyB (m1,m2) --scalePGk c polyB s
    appliedTR = rotateAroundPGa c appliedT r
    appliedTRS = scalePGk c appliedTR s
    in (unsignedDistGG polyA appliedTRS)**2
  g [m1,m2,r,s] = res where
    [m1',m2',r',s'] = combGGC polyA polyB c [m1,m2,r,s]
    res = [k1*m1', k1*m2', k2*r', k3*s']
  dir = neg' $ normalize' $ g cumulative
  [m1', m2', r', s'] = valof' 4 $ linesearch f g dir cumulative
  delt = [m1', m2', r', s']
  appliedT = movebyGm polyB (cumT1+m1', cumT2+m2') --scalePGk c polyB s
  appliedTR = rotateAroundPGa c appliedT (cumR+r')
  appliedTRS = scalePGk c appliedTR (cumS+s')
  g0 = mag' $ g $ add' cumulative delt
  in (appliedTRS, if f (add' cumulative delt) < epsilon then 0 else g0, add' cumulative delt, c)

energyOutsideGradGGC :: Polygon -> Polygon -> Point -> [Double] -> [Double]
energyOutsideGradGGC polyA polyB c cumulative = 
  dsqIntegralGradGGC polyA polyB c cumulative True
  
energyOutsideGGC :: Polygon -> Polygon -> Point -> [Double] -> Double
energyOutsideGGC polyA polyB c cumulative = 
  dsqIntegralGGC polyA polyB c cumulative True

energyInsideGradGGC :: Polygon -> Polygon -> Point -> [Double] -> [Double]
energyInsideGradGGC polyA polyB c cumulative = 
  dsqIntegralGradGGC polyA polyB c cumulative False
  
energyInsideGGC :: Polygon -> Polygon -> Point -> [Double] -> Double
energyInsideGGC polyA polyB c cumulative = 
  dsqIntegralGGC polyA polyB c cumulative False

dsqIntegralGGC :: Polygon -> Polygon -> Point -> [Double] -> Bool -> Double
dsqIntegralGGC polyA polyB c [mx,my,t,s] fromOutside = let
  appliedT = movebyGm polyB (mx,my) --scalePGk c polyB s
  appliedTR = rotateAroundPGa c appliedT t
  appliedTRS = scalePGk c appliedTR s
  (inside, outside) = cookiePoly polyA appliedTRS
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ 
    if fromOutside then outside else inside
  sampleSum = foldl (\a b->a+b) 0 $ map (\p->distsq p $ closestPointGP polyA p) samples
  res = sampleSum * stepInterval
  in trace ("energy: "++(show res)) res

dsqIntegralGradGGC :: Polygon -> Polygon -> Point -> [Double] -> Bool -> [Double]
dsqIntegralGradGGC polyA polyB c [mx,my,t,s] fromOutside = let
  appliedT = movebyGm polyB (mx,my) --scalePGk c polyB s
  appliedTR = rotateAroundPGa c appliedT t
  appliedTRS = scalePGk c appliedTR s
  (inside, outside) = cookiePoly polyA appliedTRS
  samples = foldl (\a b->a++b) [] $ map (sampleSeg stepInterval) $ 
    if fromOutside then outside else inside
  closestPs = map (closestPointGP polyA) samples
  samplesOrig = map (\p -> let
    revS = scalePPk c p (1/s)
    revSR = rotateAroundPPa c revS (-t)
    in movebyPm revSR (-mx,-my)) samples
  zp = zip closestPs samplesOrig
  res = mult' ((fromIntegral $ length samplesOrig)/stepInterval) 
    $ foldl add' [0,0,0,0] 
    $ map (\(a,b)->reduceDistGCPbPa polyA c b a [mx,my,t,s]) zp
  in res

-- returns a list of sample points along segment, each separated by stepsize pixels
sampleSeg :: Double -> LineSeg -> [Point]
sampleSeg stepsize (x,y) = let
  len = dist x y
  samplets = [0,(stepsize/len)..1]
  in map (fromT (x,y)) samplets
  
-- input: polyA, c, pt on A, and pt on B that: we know after the cumulative transformation it's closest to Pa.
reduceDistGCPbPa :: Polygon -> Point -> Point -> Point -> [Double] -> [Double]
reduceDistGCPbPa poly (cx,cy) (x1,x2) (v1,v2) [mx, my, t, s] = let
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
  -- pen = -1/s**2
  in [dmx, dmy, dt, ds]

-- this version of encouraging containment: uses integration of distsq along edge as energy
containGGCout :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> (Polygon, Double, [Double])
containGGCout polyA polyB c cumulative [k1, k2, k3] = let
  [cumT1, cumT2, cumR, cumS] = cumulative
  f = energyOutsideGGC polyA polyB c
  g cumulative = let 
    [m1',m2',r',s'] = energyOutsideGradGGC polyA polyB c cumulative
    in [k1*m1', k1*m2', k2*r', k3*s']
  dir = neg' $ normalize' $ g cumulative 
  [m1', m2', r', s'] = valof' 4 $ linesearch f g dir cumulative
  delt = [m1', m2', r', s']
  appliedT = movebyGm polyB (cumT1+m1', cumT2+m2') --scalePGk c polyB s
  appliedTR = rotateAroundPGa c appliedT (cumR+r')
  appliedTRS = scalePGk c appliedTR (cumS+s')
  g0 = mag' $ g $ add' cumulative delt
  in (appliedTRS, if f (add' cumulative delt) < epsilon then 0 else g0, add' cumulative delt)

{-

-- uses max signed distance (sq) as energy
containGGC :: Polygon -> Polygon -> Point -> [Double] -> [Double]
containGGC polyA polyB (cx,cy) [mx, my, t, s] = let
  appliedT = movebyGm polyB (mx,my) --scalePGk c polyB s
  appliedTR = rotateAroundPGa (cx,cy) appliedT t
  appliedTRS = scalePGk (cx,cy) appliedTR s
  ((x1',y1), (x2',y2)) = maxSignedDistSegGG polyA appliedTRS
  fromA = (dist (x1',y1) $ closestPointGP polyA (x1',y1)) < epsilon
  ((v1,v2), (x1'',x2'')) = if fromA then ((x1',y1),(x2',y2)) else ((x2',y2),(x1',y1))
  revS = scalePPk (cx,cy) (x1'',x2'') (1/s)
  revSR = rotateAroundPPa (cx,cy) revS (-t)
  (x1,x2) = movebyPm revSR (-mx,-my)
  in reduceDistGCPbPa polyA (cx,cy) (x1,x2) (v1,v2) [mx,my,t,s]

-- version below: uses max signed dist sq as energy
containGGCout' :: Polygon -> Polygon -> Point -> [Double] -> [Double] -> (Polygon, Double, [Double])
containGGCout' polyA polyB c cumulative [k1, k2, k3] = let
  [cumT1, cumT2, cumR, cumS] = cumulative
  f [m1,m2,r,s] = let 
    appliedT = movebyGm polyB (m1,m2) --scalePGk c polyB s
    appliedTR = rotateAroundPGa c appliedT r
    appliedTRS = scalePGk c appliedTR s
    sd = maxSignedDistGG polyA appliedTRS
    in if sd<0 then -(sd**2) else sd**2
  g [m1,m2,r,s] = let
    [m1',m2',r',s'] = containGGC polyA polyB c [m1,m2,r,s]
    in [k1*m1', k1*m2', k2*r', k3*s']
  --dir = neg' $ normalize' $ g cumulative
  --dir = direction1 f cumulative
  dir = direction2 polyA polyB c cumulative -- use the newly defined quantity only as search direction,
                                            -- actual energy & gradient still use max signed dist
  [m1', m2', r', s'] = valof' 4 $ linesearch' f g dir cumulative
  delt = [m1', m2', r', s']
  appliedT = movebyGm polyB (cumT1+m1', cumT2+m2') --scalePGk c polyB s
  appliedTR = rotateAroundPGa c appliedT (cumR+r')
  appliedTRS = scalePGk c appliedTR (cumS+s')
  g0 = mag' $ g $ add' cumulative delt
  in (appliedTRS, if f (add' cumulative delt) < epsilon then 0 else g0, add' cumulative delt)

-- trying different helpers for finding search direction to minimize max signed distance.

-- (translation only) uniformly sample 32 directions in 2D, pick the one that lowers the energy fastest
direction1 f [cumT1, cumT2, cumR, cumS] = let
  dirSamples = [0,pi/16..2*pi]
  dir2D = foldl (\dir1 dir2-> let
    f1 = f [cumT1+5*cos(dir1), cumT2+5*sin(dir1), cumR, cumS]
    f2 = f [cumT1+5*cos(dir2), cumT2+5*sin(dir2), cumR, cumS]
    in if f1<f2 then dir1 else dir2) (1/0) dirSamples
  in normalize' [cos(dir2D), sin(dir2D), 0, 0] 

direction2 polyA polyB c cumulative = let
  [m1',m2',r',s'] = neg' $ normalize' $ energyOutsideGradGGC polyA polyB c cumulative
  in [m1',m2',r',s']

-}
