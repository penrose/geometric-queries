module Polygons (
  -- for debug
  unitSq,
  unitSq2,
  ---- from this Polygons module ----
  Polygon,
  outsidedness,
  getSegments,
  closestPointGP,
  signedDist,
  segIsInside,
  shortestSegmentGS,
  maxUDistSegGS,
  shortestDistGS,
  shortestSegmentGG,
  maxUDistSegGG,
  unsignedDistGG,
  minSignedDistSegGG,
  maxSignedDistSegGG,
  -- for testing --
  maxUDistGG,
  maxUDistGGtestSeg,
  maxUDistGGtest,
  maxUDistSegGSaprx
) where

import PointsAndLines
import Data.List

type Polygon = [Point]

unitSq = [(0,0),(0,1),(1,1),(1,0)]::Polygon
unitSq2 = [(0.5,0.5),(1.5,0.5),(1.5,1.5),(0.5,1.5)]::Polygon

-- (helper) bounding box, represented by the diagonal line segment
bbox :: Polygon -> LineSeg
bbox pts = let
  xs = map (\(x,y)->x) pts
  ys = map (\(x,y)->y) pts
  in ((minimum xs, minimum ys), (maximum xs, maximum ys))

-- (helper) returns a point outside of a polygon 
-- very arbitrary rn. Should use some randomness...
pOutside :: Polygon -> Point
pOutside pts = let ((x1,y1),(x2,y2)) = bbox pts in (x1-3, y1-5)

-- (helper) returns a list of LineSeg that makes up a polygon
getSegments :: Polygon -> [LineSeg]
getSegments pts = let 
  lastInd = length pts - 1
  f x = if x==lastInd then (pts!!lastInd, pts!!0) else (pts!!x, pts!!(x+1))
  in map f [0..lastInd]

-- (deprecated) returns -1 if a given point is in the given polygon, 1 otherwise
-- could bump into weird edge cases esp if polygon has many vertices
outsidednessOld :: Polygon -> Point -> Double
outsidednessOld pts p = let
  raySeg = (p, pOutside pts)
  ixs = map (intersectionSS raySeg) (getSegments pts)
  f count ix = case ix of
    Nothing -> count
    Just _ -> count + 1
  ixCount = foldl f 0 ixs
  in if mod ixCount 2 == 0 then 1.0 else -1.0

-- returns -1.0 if given point is inside polygon or on its boundary, 1.0 otherwise
outsidedness :: Polygon -> Point -> Double
outsidedness pts (x0,y0) = 
  if dist (x0,y0) (closestPointGP pts (x0,y0))<epsilon then 0.0
  else let
  diffp = map (\(x,y)->(x-x0,y-y0)) pts
  getAngle (x,y) = atan2 y x 
  sweeps = map (\(p1,p2)->(getAngle p2)-(getAngle p1)) (getSegments diffp)
  adjust sweep = 
    if sweep>pi then sweep-2*pi
    else if sweep<(-pi) then 2*pi+sweep
    else sweep
  sweepAdjusted = map adjust sweeps
  -- if inside pos poly, res would be 2*pi,
  -- if inside neg poly, res would be -2*pi,
  -- else res would be 0
  res = foldl (+) 0.0 sweepAdjusted
  in if res>pi || res<(-pi) then -1.0 else 1.0

-- returns the closest point to p on a polygon's boundary
closestPointGP :: Polygon -> Point -> Point
closestPointGP pts p = let 
  points = map (closestPointPS p) (getSegments pts)--closest point to each segment
  closer p1 p2 = (dist p p1) < (dist p p2)
  in foldl
    (\(x1,y1) (x2,y2)->if closer (x1,y1) (x2,y2) then (x1,y1) else (x2,y2)) 
    (points!!0) points

-- returns the signed distance between a polygon and a point
signedDist :: Polygon -> Point -> Double
signedDist pts p = (dist p $ closestPointGP pts p) 
  * (outsidedness pts p)

-- returns True if given segment is completely inside polygon, False otherwise
segIsInside :: Polygon -> LineSeg -> Bool
segIsInside pts (p1, p2) = let
  endPointsInside = outsidedness pts p1 == -1 && outsidedness pts p2 == -1
  ixs = map (intersectionSS (p1,p2)) (getSegments pts)
  f noIx ix = case ix of
    Nothing -> noIx
    Just _ -> False
  in endPointsInside && foldl f True ixs

-- (helper) returns the shorter of the two
shorterSeg :: LineSeg -> LineSeg -> LineSeg
shorterSeg (p1, p2) (p3, p4) = case (dist p3 p4) - (dist p1 p2) > 0 of
  True -> (p1, p2)
  False -> (p3, p4)

-- (helper) opposite of shorterSeg
longerSeg :: LineSeg -> LineSeg -> LineSeg
longerSeg (p1, p2) (p3, p4) = case (dist p3 p4) - (dist p1 p2) < 0 of
  True -> (p1, p2)
  False -> (p3, p4)

-- returns the shortest segment connecting 
-- a point on polygon boundary and a point on input segment
shortestSegmentGS :: Polygon -> LineSeg -> LineSeg
shortestSegmentGS pts seg = foldl shorterSeg infSeg $ 
  map (shortestSegmentSS seg) (getSegments pts)

-- (helper) (sampling) D_{A,s} (max unsigned dist from a poly to a segment)
maxUDistSegGSaprx :: Polygon -> LineSeg -> LineSeg
maxUDistSegGSaprx pts ((x1,y1), (x2,y2)) = let
  density = (dist (x1,y1) (x2,y2)) * 5
  stepx = (x2-x1) / density
  stepy = (y2-y1) / density
  indices = [0..density]
  samplesRaw = map (\i->(x1+i*stepx, y1+i*stepy)) indices
  samples = filter (\(x,_)->(x-x1)/(x2-x1)<=1) samplesRaw
  in foldl longerSeg zeroSeg $ map (\p->(closestPointGP pts p, p)) samples

-- (helper) (exact) D_{A,s} (max unsigned dist from a poly to a segment)
maxUDistSegGS :: Polygon -> LineSeg -> LineSeg
maxUDistSegGS pts seg = let
  (p1, p2) = seg
  boundary = getSegments pts
  pairs = concat $ map (\s->map (\t->(s,t)) boundary) boundary
  -- candidates: polygon vertices
  ixnorml = map (ixNorm seg) boundary
  maybeCpsV = foldl (++) [] ixnorml
  -- candidates: equidistant points
  maybeCps = foldl (++) [] $ map (equiDistPts seg) pairs
  f x = case x of Nothing -> False
                  Just _ -> True
  cps = map (\x->case x of Just p->p) $ filter f (maybeCps ++ maybeCpsV)
  -- removes duplicates from cps
  cpsNoDup = map head $ group $ sort cps
  -- get two sets of candidate segments: involving / not involving vertices
  swap f x y = f y x
  -- candidatesOnV = map (swap longestSegmentSS seg) boundary
  candidatesOnV = [(p1, closestPointGP pts p1), (p2, closestPointGP pts p2)]
  candidatesOnE = map (\p->(p, closestPointGP pts p)) cpsNoDup
  in foldl longerSeg zeroSeg $ candidatesOnV ++ candidatesOnE

-- returns the shortest unsigned distance between a polygon and a segment
shortestDistGS :: Polygon -> LineSeg -> Double
shortestDistGS pts seg = let 
  (p1, p2) = shortestSegmentGS pts seg
  in dist p1 p2

-- returns the shortest segment connecting two polygons' boundaries
shortestSegmentGG :: Polygon -> Polygon -> LineSeg
shortestSegmentGG pts1 pts2 =
  foldl shorterSeg infSeg $ map (shortestSegmentGS pts1) (getSegments pts2)
  
-- returns the shortest unsigned distance between two polygons (their boundaries)
unsignedDistGG :: Polygon -> Polygon -> Double
unsignedDistGG pts1 pts2 = let 
  (p1, p2) = shortestSegmentGG pts1 pts2
  in dist p1 p2

-- Length of result is D_{A,B} (max unsigned dist).
maxUDistSegGG :: Polygon -> Polygon -> LineSeg
maxUDistSegGG pts1 pts2 = 
  foldl longerSeg zeroSeg $ map (maxUDistSegGS pts1) (getSegments pts2)

-- D_{A,B} max unsigned distance between polygons A and B
maxUDistGG :: Polygon -> Polygon -> Double
maxUDistGG pts1 pts2 = let
  (p1, p2) = maxUDistSegGG pts1 pts2
  in dist p1 p2

-- gives D_{A,B} from sampling method
maxUDistGGtest :: Polygon -> Polygon -> Double
maxUDistGGtest pts1 pts2 = let
  (p1, p2) = foldl longerSeg zeroSeg $ 
    map (maxUDistSegGSaprx pts1) (getSegments pts2)
  in dist p1 p2

-- length of output is D_{A,B} from sampling method
maxUDistGGtestSeg pts1 pts2 = foldl longerSeg zeroSeg $
  map (maxUDistSegGSaprx pts1) (getSegments pts2)

-- (helper) for cookiePoly
cookieSeg :: Polygon -> LineSeg -> ([LineSeg], [LineSeg])
cookieSeg pts (p1,p2) = let
  ixs = map (intersectionSS (p1,p2)) (getSegments pts)
  f ix = case ix of
    Nothing -> False
    Just _ -> True
  ixsFilter = filter f ixs
  g ix = case ix of
    Just x -> x
  cutPts = p1:p2:(map g ixsFilter)
  cmp q1 q2 = compare (dist p1 q1) (dist p1 q2)
  cutPtsSorted = sortBy cmp cutPts
  allSegments = tail $ 
    map (\i->if i==0 then ((0,0),(0,0)) 
    else (cutPtsSorted!!i,cutPtsSorted!!(i-1))) [0..((length cutPts)-1)]
  inside (p1,p2) = outsidedness pts p1<0 || outsidedness pts p2<0
  (l1, l2) = partition inside allSegments
  in (filter (\(p1,p2)->dist p1 p2>epsilon) l1, 
      filter (\(p1,p2)->dist p1 p2>epsilon) l2)

-- use polyA as cookie cutter, divide polyB into segments inside and outside of A
cookiePoly :: Polygon -> Polygon -> ([LineSeg], [LineSeg])
cookiePoly polyA polyB = let
  -- cut all segments of B with A
  raw = map (\seg->cookieSeg polyA seg) (getSegments polyB)
  in foldl (\(l1,l2) (l3,l4)->(l1++l3,l2++l4)) ([],[]) raw

minSignedDistSegGG :: Polygon -> Polygon -> LineSeg
minSignedDistSegGG polyA polyB = let
  (inside, outside) = cookiePoly polyA polyB
  (p1,p2) = foldl shorterSeg infSeg $ map (shortestSegmentGS polyA) outside
  (p3,p4) = foldl longerSeg zeroSeg $ map (maxUDistSegGS polyA) inside
  lenIn = dist p3 p4
  in if lenIn>0 then (p3,p4) else (p1,p2)

maxSignedDistSegGG :: Polygon -> Polygon -> LineSeg
maxSignedDistSegGG polyA polyB = let
  (inside, outside) = cookiePoly polyA polyB
  (p1,p2) = foldl longerSeg zeroSeg $ map (maxUDistSegGS polyA) outside
  (p3,p4) = foldl shorterSeg infSeg $ map (shortestSegmentGS polyA) inside
  lenOut = dist p1 p2
  in if lenOut>0 then (p1,p2) else (p3,p4)
