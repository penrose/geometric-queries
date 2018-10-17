module Polygons (
  ---- from this Polygons module ----
  outsidedness,
  closestPointGP,
  signedDist,
  segIsInside,
  shortestSegmentGS,
  shortestDistGS,
  shortestSegmentGG,
  shortestDistGG
) where

import PointsAndLines

type Polygon = [Point]

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

-- returns -1 if a given point is in the given polygon, 1 otherwise
-- could bump into weird edge cases esp if polygon has many vertices
outsidedness :: Polygon -> Point -> Int
outsidedness pts p = let
  raySeg = (p, pOutside pts)
  ixs = map (intersectionSS raySeg) (getSegments pts)
  f count ix = case ix of
    Nothing -> count
    Just _ -> count + 1
  ixCount = foldl f 0 ixs
  in if mod ixCount 2 == 0 then 1 else -1

outsidednessAlt :: Polygon -> Point -> Double
outsidednessAlt pts (x0,y0) = let
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
  in res

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
  * (fromIntegral $ outsidedness pts p)

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

-- (helper) a very long segment
infSeg = ((-1/0, 0), (1/0, 0))

-- returns the shortest segment connecting 
-- a point on polygon boundary and a point on input segment
shortestSegmentGS :: Polygon -> LineSeg -> LineSeg
shortestSegmentGS pts seg = foldl shorterSeg infSeg $ 
  map (shortestSegmentSS seg) (getSegments pts)
  
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
shortestDistGG :: Polygon -> Polygon -> Double
shortestDistGG pts1 pts2 = let 
  (p1, p2) = shortestSegmentGG pts1 pts2
  in dist p1 p2

