module PointsAndLines (
  ------ helpers for debug------
  -- Line(..),
  -- line,
  -- normalLine,
  -- intersectionLL,
  -- isOnLine,
  longestSegmentSS,
  isEndpoint,
  epsilon,
  infSeg,
  zeroSeg,
  getT,
  fromT,
  ----- exports -----
  Point,
  LineSeg,
  dist,
  distsq,
  isOnLineSeg,
  closestPointPS,
  shortestDistPS,
  intersectionSS,
  shortestDistSS,
  shortestSegmentSS,
  mudCandRel,
  ixNorm,
  equiDistPts
) where

import Data.List

-- (utility) seems like fay doesn't have this
isInf :: Double -> Bool
isInf x = x == 1/0 || x == -1/0

-- epsylon for checking fp equality
epsilon = 0.1 ** 8
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

-- Vector represented as a tuple
type Vector = (Double, Double)

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

-- (helper) returns the normal line of the given line
normalLine :: Line -> Line
normalLine l = Line {
  slope = -1 / (slope l), -- could be +/-0, posInf, negInf
  point = point l
}

-- * (helper) returns True if given point is on the line (as defined by the segment)
isOnLine :: Point -> LineSeg -> Bool
isOnLine (x, y) ((x1, y1), (x2, y2)) = let
  m = slope $ line ((x1, y1), (x2, y2))
  m' = slope $ line ((x1, y1), (x, y))
  m'' = slope $ line ((x2, y2), (x, y))
  in (almostEq m m' || almostEq m m'') 

-- reprensentation of line with normal & offset
data LineN = LineN {
  normalVec :: Vector,
  offsetD :: Double
} deriving (Show)

-- constructor for LineN from two points
lineN :: LineSeg -> LineN
lineN (p1, p2) = let
  l = line (p1, p2)
  n = Line {
    slope = -1 / (slope l),
    point = (0.0, 0.0)
  }
  (ixx, ixy) = case intersectionLL l n of Just (a,b) -> (a,b)
  d = dist (0,0) (ixx, ixy)
  -- (u1, u2) = (ixx/d, ixy/d)
  (u1, u2) = if d<epsilon then let
    (_, (nx, ny)) = lineToSeg n
    len = dist (0,0) (nx, ny)
    in (nx/len, ny/len) else (ixx/d, ixy/d)
  offD = if d<epsilon then 0 else -(u1*ixx + u2*ixy)
  in LineN {normalVec = (u1, u2), offsetD = offD}

-- (helper) sanity check for LineN
isOnLineN :: Point -> LineSeg -> Bool
isOnLineN (x, y) seg = let
  l = lineN seg
  (u1, u2) = normalVec l
  d = offsetD l
  in abs (u1 * x + u2 * y + d) < epsilon

-- (helper) given a pt, return t corresponding to its vert. proj. on segment xy.
-- ONLY WORKS WHEN pt's PROJ IS ON xy
getT :: LineSeg -> Point -> Double
getT (x,y) pt = let
  l = line (x,y)
  nl = Line { slope = -1/(slope l), point = pt}
  ix = case intersectionLL l nl of Just pt -> pt
  in (dist x ix) / (dist x y)

-- (helper) given t, return corresponding point on xy
fromT :: LineSeg -> Double -> Point
fromT ((x1,y1), (x2,y2)) t = let
  dx = x2 - x1
  dy = y2 - y1
  in (x1+t*dx, y1+t*dy)

-- returns distance squared bt. two points
distsq :: Point -> Point -> Double
distsq (x1, y1) (x2, y2) = (x2-x1)^2 + (y2-y1)^2

-- returns the distance between two points
dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

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

-- helper for max unsigned (to check vertices of polygons)
ixNorm :: LineSeg -> LineSeg -> [Maybe Point]
ixNorm b (a1, a2) = let
  lb = line b
  la = line (a1, a2)
  n1 = Line {
    slope = -1 / (slope la),
    point = a1
  }
  n2 = Line {
    slope = -1 / (slope la),
    point = a2
  }
  ix1 = case intersectionLL lb n1 of 
    Just ix -> (if isOnLineSeg ix b then Just ix else Nothing)
    Nothing -> Nothing
  ix2 = case intersectionLL lb n2 of 
    Just ix -> (if isOnLineSeg ix b then Just ix else Nothing)
    Nothing -> Nothing
  in [ix1, ix2]

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

-- (helper) ASYMMETRICAL. Connects the pt on seg2 furthest 
-- (wrt. unsigned dist) from seg1 to seg1
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

-------- (deprecated) sketchy, maybe incorrect sol for max unsigned --------

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


-------------- Keenan's way for max unsigned --------------

testSS = (((0.8,1.4),(6.5,-10)), ((0,-1), (10,2))) :: (LineSeg, LineSeg)
seg = ((2, 10), (5, -4)) :: LineSeg

isEndpoint :: Point -> LineSeg -> Bool
isEndpoint (x1, y1) ((x2, y2), (x3, y3)) =
  (abs(x2-x1)<epsilon && abs(y2-y1)<epsilon) ||
  (abs(x3-x1)<epsilon && abs(y3-y1)<epsilon)

tsb = ((404,320),(500,361)) :: LineSeg
tsa1 = ((387,392),(528,399)) :: LineSeg
tsa2 = ((528,399),(408,473)) :: LineSeg
tsa3 = ((408,473),(387,392)) :: LineSeg

-- solves for *possible* equidistant points on seg b to two segments a1, a2
-- by checking: 
--   equidist. to somewhere mid of a1 and somewhere mid of a2
--   equidist. to mid of a1 and endpoint of a2, and vice versa
--   equidist. to endpoint of a1 and endpoint of a2
equiDistPts :: LineSeg -> (LineSeg, LineSeg) -> [Maybe Point]
equiDistPts ((x1,y1), (x2,y2)) (seg1, seg2) = let
  ln1 = lineN seg1
  (u1, u2) = normalVec ln1
  c = offsetD ln1
  ln2 = lineN seg2
  (v1, v2) = normalVec $ lineN seg2
  d = offsetD ln2
  a = x2 - x1
  b = y2 - y1
  -- solve for concave openings (of both dirs)
  num1 = v1*x1 + v2*y1 + d - u1*x1 -u2*y1 - c
  denom1 = u1*a + u2*b - v1*a - v2*b
  t1 = num1 / denom1
  -- negate u and solve again
  num2 = v1*x1 + v2*y1 + d + u1*x1 +u2*y1 + c
  denom2 = -u1*a - u2*b - v1*a - v2*b
  t2 = num2 / denom2
  -- yes there could be 2 equidist points
  (ix1, ix2) = ((x1+t1*a, y1+t1*b), (x1+t2*a, y1+t2*b))
  ---- now solve for equidist to mid seg vs. an endpoint
  (a1, a2) = seg1
  (a3, a4) = seg2
  -- solve for equidist. bt. segment1 (mid) and segment2 (end1)
  (t3, t4) = solverSP (x1,y1,a,b) (u1,u2,c) a3
  -- solve for equidist. bt. segment1 (mid) and segment2 (end2)
  (t5, t6) = solverSP (x1,y1,a,b) (u1,u2,c) a4
  -- solve for equidist. bt. segment2 (mid) and segment1 (end1)
  (t7, t8) = solverSP (x1,y1,a,b) (v1,v2,d) a1
  -- solve for equidist. bt. segment2 (mid) and segment1 (end2)
  (t9, t10) = solverSP (x1,y1,a,b) (v1,v2,d) a2
  ---- also solve for equidist to two points
  bseg = ((x1,y1),(x2,y2))
  t11 = solverPP bseg a1 a3
  -- t11 = let tt = solverPP bseg a1 a3 in if isNaN tt then 0 else tt
  t12 = solverPP bseg a1 a4
  t13 = solverPP bseg a2 a3
  t14 = solverPP bseg a2 a4
  -- gather all the ts, validate and return corresponding points
  ts = [t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14]
  validate t = if t<0 || t>1 then Nothing else Just (x1+t*a, y1+t*b)
  -- check if related points are on segments
  cp1 = closestPointPS ix1 seg1
  cp2 = closestPointPS ix1 seg2
  cp3 = closestPointPS ix2 seg1
  cp4 = closestPointPS ix2 seg2
  -- return result
  res1 = if isEndpoint cp1 seg1 || isEndpoint cp2 seg2
    || t1<=0 || t1>=1 then Nothing else Just ix1
  res2 = if isEndpoint cp3 seg1 || isEndpoint cp4 seg2
    || t2<=0 || t2>=1 then Nothing else Just ix2
  in res1:res2:(map validate ts)

-- (helper) solves for t corresponding equidist. pt on B to a1 and an endpoint of a2
solverSP (x1,y1,a,b) (u1,u2,offset) (a3x,a3y) = let
  c = -offset
  rt1 = 4*(a3y*b - a*c*u1 + a*u1^2*x1 + a*u2*u1*y1 - a*x1 + a*a3x - b*c*u2 + b*u2*u1*x1
        + b*u2^2*y1 - b*y1)^2 - 4*(a^2*u1^2 - a^2 + 2*a*b*u1*u2 + b^2*u2^2 - b^2) * 
        (2*a3x*x1 + 2*a3y*y1 - a3x^2 - a3y^2 + c^2 - 2*c*u1*x1 - 
        2*c*u2*y1 + 2*u1*u2*x1*y1 + u1^2*x1^2 + u2^2*y1^2 - x1^2 - y1^2)
  nb1 = 2 * (-a3y*b + a*c*u1 - a*u1^2*x1 - a*u2*u1*y1 + a*x1 - 
        a*a3x + b*c*u2 - b*u2*u1*x1 - b*u2^2*y1 + b*y1)
  denom_2a = 2 * (a^2*u1^2 - a^2 + 2*a*b*u1*u2 + b^2*u2^2 - b^2)
  in if rt1 < 0 then (-1, -1) else let
    rt1_sqrt = sqrt rt1
    t3 = (nb1 + rt1_sqrt) / denom_2a
    t4 = (nb1 - rt1_sqrt) / denom_2a
    in (if isNaN t3 then -1 else t3, 
        if isNaN t4 then -1 else t4)

-- (helper) solves for t corresponding equidist. pt. on B to points a1 and a2
solverPP (b1, b2) a1 a2 = if dist a1 a2 < epsilon then -1 else let
  ((x0, y0), (xx, yy)) = (b1, b2)
  ((x1, y1), (x2, y2)) = (a1, a2)
  a = xx-x0
  b = yy-y0
  mid = (x1+(x2-x1)/2, y1+(y2-y1)/2)
  lb = line (b1, b2)
  la = Line {
    slope = -1 / (slope $ line (a1,a2)),
    point = mid
  }
  ix = intersectionLL lb la
  t0 = case ix of
    Nothing -> -1
    Just (ixx, ixy) -> let
      tx = (ixx-x0) / a
      ty = (ixy-y0) / b
      in if not $ isInfinite tx then tx else ty
  guard = a*x1 + b*y1 - a*x2 - b*y2 > epsilon -- midline of a1,a2 not exactly lb
  in if not guard then t0 else case intersectionLL lb (line ((x1,y1),(x2,y2))) of
      Just (ixx,ixy) -> let
        tx = (ixx-x0) / a
        ty = (ixy-y0) / b
        in if not $ isInfinite tx then tx else ty
