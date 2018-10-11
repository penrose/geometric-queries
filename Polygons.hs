module Polygons (
-- not done yet so nothing exported
) where

import PointsAndLines

type Polygon = [Point]

-- (helper) bounding box
bbox :: Polygon -> LineSeg
bbox pts = let
  xs = map (\(x,y)->x) pts
  ys = map (\(x,y)->y) pts
  in ((minimum xs, minimum ys), (maximum xs, maximum ys))

-- (helper) returns a point outside of a polygon 
-- very arbitrary rn. would be helpful if could introduce some randomness?
pOutside :: Polygon -> Point
pOutside pts = let ((x1,y1),(x2,y2)) = bbox pts in (x1-3, y1-5)

-- (helper) returns a list of LineSeg that makes up a polygon
getSegments :: Polygon -> [LineSeg]
getSegments pts = let 
  lastInd = length pts - 1
  cyc = cycle pts 
  f x = (cyc!!x, cyc!!(x+1))
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

-- returns the closest point to p on a polygon's boundary
closestPointGP :: Polygon -> Point -> Point
closestPointGP pts p = minimum $ map (closestPointPS p) (getSegments pts)

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
