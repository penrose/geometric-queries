-- Currently fp edge cases (div by 0, Infinity) not handled
module PointsAndLines (
  -- helpers for debug
  Line(..),
  line,
  normalLine,
  intersectionLL,
  isOnLine,
  -- exports
  Point,
  LineSeg,
  dist,
  isOnLineSeg,
  closestPointPS,
  shortestDistPS,
  intersectionSS,
  shortestDistSS,
  shortestSegmentSS
) where

-- epsylon for checking fp equality
epsilon = 0.000000001
-- helper for checking slope equality *ONLY*
almostEq a b = (isInfinite a && isInfinite b) || abs (a-b) < epsilon

posInf = 1 / 0
negInf = -1 / 0
infSeg = ((negInf,negInf), (posInf,posInf))

-- arbitrary constant(s)
c = 4

-- Point represented as a tuple
type Point = (Double, Double)

-- Line segment represented by two points
type LineSeg = (Point, Point)

-- Line in point-slope form
data Line = Line {
  slope :: Double,
  point :: (Double, Double)
} deriving (Show)

-- constructor from two points
-- REQUIRES: segment doesn't have 0 length
line :: LineSeg -> Line
line ((x1,y1), (x2,y2)) = Line {
  slope = let
    dy = y2 - y1 -- finite
    dx = x2 - x1 -- finite
    in dy / dx, -- NaN if given segment has 0 length (0 / 0)
  point = (x1, y1)
}

-- (helper) returns two points (a LineSeg) on a Line
lineToSeg :: Line -> LineSeg
lineToSeg l = let
  dx = 4 -- quite arbitrary
  m = slope l
  (x1, y1) = point l
  y2 = y1 + m * dx
  in ((x1, y1), -- original point
    (if isInfinite y2 then x1 else x1+dx, -- mind if l is almost vertical
     if isInfinite y2 then y1+c else y2))


-- returns the distance between two points
dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

-- (helper) returns the normal line of the given line
normalLine :: Line -> Line
normalLine l = Line {
  slope = -1 / (slope l), -- could be +/-0, posInf, negInf
  point = point l
}

-- * (helper) returns the intersection pt of two lines 
intersectionLL :: Line -> Line -> Maybe Point
intersectionLL l1 l2 = let
  (x1, y1) = point l1
  m1 = slope l1
  (x2, y2) = point l2
  m2 = slope l2
  -- handle case when two lines have very close slope (both infinite / small diff)
  in if almostEq m1 m2 then 
    case isOnLine (point l1) (lineToSeg l2) of 
      True -> Just (x1, y1)
      False  -> Nothing 
  else if isInfinite m1 then Just (x1, y2 + m2 * (x1-x2)) -- l1 is vertical
  else if isInfinite m2 then Just (x2, y1 + m1 * (x2-x1)) -- l2 is vertical
  -- l1 and l2 not parallel and both have finite slope
  else let
  num = m1*x1 - m2*x2 + y2 - y1
  denom = m1-m2 -- shouldn't be inf or NaN
  x = num / denom
  y = y1 + m1 * (x - x1)
  in Just (x, y)

-- * (helper) returns True if given point is on the line (as defined by the segment)
isOnLine :: Point -> LineSeg -> Bool
isOnLine (x, y) ((x1, y1), (x2, y2)) = let
  m = slope $ line ((x1, y1), (x2, y2))
  m' = slope $ line ((x1, y1), (x, y))
  m'' = slope $ line ((x2, y2), (x, y))
  in (almostEq m m' || almostEq m m'') 

-- * returns True if the point is on the line segment, False otherwise
isOnLineSeg :: Point -> LineSeg -> Bool
isOnLineSeg (x, y) ((x1, y1), (x2, y2)) = 
  isOnLine (x, y) ((x1, y1), (x2, y2)) && 
  ((x >= x1 && x <= x2) || (x >= x2 && x <= x1)) &&
  ((y >= y1 && y <= y2) || (y >= y2 && y <= y1))

-- * returns the point on a line segment closest to the given point
closestPointPS :: Point -> LineSeg -> Point
closestPointPS (x, y) ((x1, y1), (x2, y2)) = let
  l = line((x1,y1), (x2,y2))
  n = Line {
        slope = -1 / (slope l), -- could be inf or 0 but not NaN
        point = (x, y)
      }
  (ix, iy) = case intersectionLL l n of Just (ix, iy) -> (ix, iy)
  d1 = dist (x,y) (x1,y1)
  d2 = dist (x,y) (x2,y2)
  in if isOnLineSeg (ix, iy) ((x1,y1), (x2,y2)) then (ix, iy) else
     if d1 < d2 then (x1, y1) else (x2, y2)

-- * returns the closest distance between a point and a line segment
shortestDistPS :: Point -> LineSeg -> Double
shortestDistPS (x, y) ((x1, y1), (x2, y2)) = 
  dist (x, y) $ closestPointPS (x, y) ((x1, y1), (x2, y2))

-- * returns the intersection point of two line segments if there is one
intersectionSS :: LineSeg -> LineSeg -> Maybe Point
intersectionSS ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = let
  l1 = line ((x1, y1), (x2, y2))
  l2 = line ((x3, y3), (x4, y4))
  in case intersectionLL l1 l2 of 
    Nothing -> Nothing
    Just (ix, iy) -> 
      if isOnLineSeg (ix, iy) ((x1, y1), (x2, y2)) &&
         isOnLineSeg (ix, iy) ((x3, y3), (x4, y4)) then Just (ix, iy)
      else Nothing

-- returns the shortest distance between two line segments
shortestDistSS :: LineSeg -> LineSeg -> Double
shortestDistSS ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = let
   seg1 = ((x1, y1), (x2, y2))
   seg2 = ((x3, y3), (x4, y4))
   in case intersectionSS seg1 seg2 of
     Just (ix, iy) -> 0.0
     Nothing -> let
       d1 = shortestDistPS (x1, y1) seg2
       d2 = shortestDistPS (x2, y2) seg2
       d3 = shortestDistPS (x3, y3) seg1
       d4 = shortestDistPS (x4, y4) seg1
       in min (min d1 d2) (min d3 d4)

-- returns a shortest line segment with each endpoint on one of two given line segments
-- currently has a bunch of repeated computation though (could've used closestPoint instead)
shortestSegmentSS :: LineSeg -> LineSeg -> LineSeg
shortestSegmentSS ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = let
  seg1 = ((x1, y1), (x2, y2))
  seg2 = ((x3, y3), (x4, y4))
  in case intersectionSS seg1 seg2 of
    Just (ix, iy) -> ((ix, iy), (ix, iy))
    Nothing -> let
      d1 = shortestDistPS (x1, y1) seg2
      d2 = shortestDistPS (x2, y2) seg2
      d3 = shortestDistPS (x3, y3) seg1
      d4 = shortestDistPS (x4, y4) seg1
      d0 = min (min d1 d2) (min d3 d4)
      in if d0==d1 then ((x1, y1), closestPointPS (x1,y1) seg2) else
         if d0==d2 then ((x2, y2), closestPointPS (x2,y2) seg2) else
         if d0==d3 then ((x3, y3), closestPointPS (x3,y3) seg1) else
         ((x4, y4), closestPointPS (x4,y4) seg1)

