-- Currently fp edge cases (div by 0, Infinity) not handled
module PointsAndLines (
  -- -- helpers for debug
  -- Line(..),
  -- line,
  -- normalLine,
  -- intersectionLL,
  -- isOnLine,
  epsilon,
  infSeg,
  zeroSeg,
  -- -- exports
  Point,
  LineSeg,
  dist,
  isOnLineSeg,
  closestPointPS,
  shortestDistPS,
  intersectionSS,
  shortestDistSS,
  shortestSegmentSS,
  mudCandRel
) where

import Data.List

-- (utility) seems like fay doesn't have this
isInf :: Double -> Bool
isInf x = x == 1/0 || x == -1/0

-- epsylon for checking fp equality
epsilon = 0.000000001
-- helper for checking slope equality *ONLY*
almostEq a b = (isInf a && isInf b) || abs (a-b) < epsilon

posInf = (1 / 0) :: Double
negInf = (-1 / 0) :: Double
infSeg = ((negInf,negInf), (posInf,posInf))
zeroSeg = ((0,0), (0,0))

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
    (if isInf y2 then x1 else x1+dx, -- mind if l is almost vertical
     if isInf y2 then y1+c else y2))


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
  else if isInf m1 then Just (x1, y2 + m2 * (x1-x2)) -- l1 is vertical
  else if isInf m2 then Just (x2, y1 + m1 * (x2-x1)) -- l2 is vertical
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

-- (helper) gives the shortest segment in a list
shortestSegmentInList ss = let
  cmb (p1,p2) (p3,p4) = if dist p1 p2 < dist p3 p4 then (p1,p2) else (p3,p4)
  in foldl cmb infSeg ss

-- (helper) opposite from above
longestSegmentInList ss = let
  cmb (p1,p2) (p3,p4) = if dist p1 p2 > dist p3 p4 then (p1,p2) else (p3,p4)
  in foldl cmb zeroSeg ss

-- returns a shortest line segment with each endpoint on one of two given line segments
shortestSegmentSS :: LineSeg -> LineSeg -> LineSeg
shortestSegmentSS (p1, p2) (p3, p4) = case intersectionSS (p1,p2) (p3,p4) of
  Just ix -> (ix, ix)
  Nothing -> let
    -- two points on seg2
    cp1 = closestPointPS p1 (p3, p4)
    cp2 = closestPointPS p2 (p3, p4)
    -- two points on seg1
    cp3 = closestPointPS p3 (p1, p2)
    cp4 = closestPointPS p4 (p1, p2)
    in shortestSegmentInList [(cp1,p1),(cp2,p2),(cp3,p3),(cp4,p4)]

-- (helper) ASYMMETRICAL. Connects the pt on seg2 furthest (wrt. unsigned dist) from seg1 to seg1
longestSegmentSS :: LineSeg -> LineSeg -> LineSeg
longestSegmentSS (p1, p2) (p3, p4) = let
    -- two points on seg1
    cp3 = closestPointPS p3 (p1, p2)
    cp4 = closestPointPS p4 (p1, p2)
    in longestSegmentInList [(cp3,p3),(cp4,p4)]

-- returns the shortest distance between two line segments
shortestDistSS :: LineSeg -> LineSeg -> Double
shortestDistSS seg1 seg2 = let
  (p1, p2) = shortestSegmentSS seg1 seg2
  in dist p1 p2



exist x = case x of
  Nothing -> False
  Just _ -> True

-- (helper) gives a piecewise function of dist of pts on seg1 to seg2
-- and x coords (in range 0 to 1) of all critical points on it
distfunc :: LineSeg -> LineSeg -> ([LineSeg], [Double])
distfunc (p1, p2) (p3, p4) = let
  -- line containing seg1
  l1 = line (p1, p2)
  -- normal of seg2 passing through p3
  l2_1 = normalLine $ line (p3, p4)
  -- normal of seg2 passing through p4
  l2_2 = normalLine $ line (p4, p3)
  -- below: two (maybe) ixs of normals of seg2 with line that contains seg1
  ixl_1 = intersectionLL l1 l2_1
  ixl_2 = intersectionLL l1 l2_2
  -- below: two (maybe) ixs of normals of seg2 with seg1
  ix_1 = case ixl_1 of
    Nothing -> Nothing
    Just p -> if isOnLineSeg p (p1, p2) then Just p else Nothing
  ix_2 = case ixl_2 of
    Nothing -> Nothing
    Just p -> if isOnLineSeg p (p1, p2) then Just p else Nothing
  -- below: (maybe) inflection critical pt
  ix_3 = let 
    n1 = Line { slope = -1 / (slope l1), point = p3 }
    n2 = Line { slope = slope n1, point = p4 }
    ix3_1 = case intersectionLL l1 n1 of Just p -> p
    ix3_2 = case intersectionLL l1 n2 of Just p -> p
    d1 = dist p3 ix3_1
    d2 = dist p4 ix3_2
    in if d1 < d2 
      then (if isOnLineSeg ix3_1 (p1, p2) then Just ix3_1 else Nothing)
      else (if isOnLineSeg ix3_2 (p1, p2) then Just ix3_2 else Nothing)
  -- get list of cps
  f mp = case mp of
    Nothing -> False
    Just _ -> True
  cmp q1 q2 = compare (dist p1 q1) (dist p1 q2)
  cps = sortBy cmp $ map (\x -> case x of Just y -> y) $ filter f
    [Just p1, Just p2, ix_1, ix_2, ix_3]
  -- get relative positions of each cp (x)
  len = dist p1 p2
  cprel = map (\cp -> (dist p1 cp) / len) cps
  -- get dist to seg2 of each cp (y)
  cpdist = map (\cp -> shortestDistPS cp (p3, p4)) cps
  in (filter (\(a, b) -> dist a b > epsilon) $
    map (\i->((cprel!!i,cpdist!!(i)), (cprel!!(i-1),cpdist!!(i-1))))
    [1..((length cps)-1)], cprel)

s1 = ((334,300),(463,371)) :: LineSeg
s2 = ((463,371),(564,303)) :: LineSeg
s = ((393,302),(507,302)) :: LineSeg

mudCandRel :: LineSeg -> (LineSeg, LineSeg) -> [Double]
mudCandRel seg (seg1, seg2) = let
  ((x1,y1),(x2,y2)) = seg
  -- get ix pts of two dist funcs
  (func1, cps1) = distfunc seg seg1
  (func2, cps2) = distfunc seg seg2
  pairs = concat $ map (\s->map (\t->(s,t)) func2) func1
  ixs = map (\x->case x of Just (a,b) -> a) $
    filter exist $
    map (\(s1,s2)->intersectionSS s1 s2) pairs
  -- get relative pos of candidate pts for max unsigned dist
  cpsrel = concat [cps1, cps2, ixs]
  cmp (x1,_) (x2,_) = compare x1 x2
  eq (x1,_) (x2,_) = abs (x1-x2) < epsilon
  cpsrelNoDup = map head $ group $ sort cpsrel
  in cpsrelNoDup
  -- among candidate pts find the furthest one

  {-
  cmb (x1,y1) (x2,y2) = if y1>y2 then (x1,y1) else (x2,y2)
  (rel, d) = foldl cmb (cpsNoDup!!0) cpsNoDup
  in ((x1 + rel*(x2-x1), y1 + rel*(y2-y1)), d)
  -}


  
