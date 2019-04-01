{-# LANGUAGE RankNTypes, AllowAmbiguousTypes, NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}

module ShapeUtils where

import MathUtils
import Debug.Trace
import Numeric.AD

type Autofloat a = (RealFloat a, Floating a, Real a, Show a, Ord a)

-- TODO: update and "store" state info in backend maybe and let frontend only visualize
{-
-- sketch code for an objective record. Idea is to store & pass indices instead of all information
data Objective a = Objective {
    shapes :: [Int],
    objective :: Int, -- list of function elsewhere
} -}

data Shape a = 
    Pt (Point a)
  | Seg (LineSeg a)
  | Poly (Polygon a)
  | Pline (Polyline a)
  | Ell (Ellipse a)
  | Rect (Rectangle a)
  deriving (Show, Read)

data ShapeNode a = Node {
    shape :: Shape a,
    origTrans :: [a], -- maybe as another record
    trans :: [a]
} deriving (Show, Read)

fromShape :: Autofloat a => Shape a -> ShapeNode a
fromShape (Pt [x,y]) = Node{ shape=Pt[0,0], origTrans=[x,y,0,1], trans=[0,0,0,1] }
fromShape (Seg (a,b)) = let 
    [midx, midy] = mult 0.5 $ b`add`a 
    mid = [midx, midy]
    in Node {
        shape = Seg(a`sub`mid, b`sub`mid),
        origTrans = [midx, midy, 0, 1],
        trans = [0,0,0,1]
    }
fromShape (Poly(bds,hs)) = let 
    [cx,cy] = centroid bds
    c = [cx,cy]
    in Node {
        shape = Poly(moveBlob bds c, map (\b->moveBlob b c) hs),
        origTrans = [cx,cy,0,1],
        trans = [0,0,0,1]
    }

toShape :: Autofloat a => ShapeNode a -> Shape a
toShape node = let
    g = shape node
    [mx,my,r,s] = combTrans (origTrans node) (trans node)
    cum = [mx,my,r,s]
    in case g of
        Pt[x,y] -> Pt[mx,my]
        Seg(a,b) -> Seg(transformP a cum, transformP b cum)
        Poly(bds,hs) -> Poly (
            transformG bds cum, 
            map (\b->transformG b cum) hs )

-- converts a shape to an object string
shape2str :: Autofloat a => Shape a -> String
shape2str shape = case shape of
    Pt p -> "{\"type\":\"Pt\", \"value\":"++(show p)++"}"
    Seg (a,b) -> "{\"type\":\"Seg\", \"value\":["++(show a)++","++(show b)++"]}"
    Poly (pts,hs) -> "{\"type\":\"Poly\", \"value\":["++(show pts)++","++(show hs)++"]}"

node2str :: Autofloat a => ShapeNode a -> String
node2str node = let
    s = shape node
    ot = origTrans node
    t = trans node
    in "{\"shape\":"++(shape2str s)++
        ",\"origTrans\":"++(show ot)++
        ",\"trans\":"++(show t)++"}"

state2str :: Autofloat a => [ShapeNode a] -> String
state2str state = case length state of 
    0 -> "[]"
    _ -> "[" ++ (node2str $ state!!0) ++
        (foldl (++) "" $ map (\n -> ","++(node2str n)) $ drop 1 state) ++ "]"

----- def and ops on normal vectors -----
type Vect a = [a]

-------- point ------------

-- array of numbers as point
type Point a = [a]

-------- segment ----------

type LineSeg a = (Point a, Point a)

-- get the normal vector
normS :: Autofloat a => LineSeg a -> Vect a
normS (p1,p2) = normalize $ rot90 $ p2 `sub` p1

-- get the length of segment
lenS :: Autofloat a => LineSeg a -> a
lenS (p1,p2) = mag $ p2 `sub` p1

-- input point, query parametric t
gettPS :: Autofloat a => Point a -> LineSeg a -> a
gettPS p (a,b) = let 
    v_ab = b `sub` a
    projl = v_ab `dot` (p `sub` a)
    in projl / (magsq $ v_ab)

-- input point, query whether t in range [0,1]
onS :: Autofloat a => Point a -> LineSeg a -> Bool
onS p s = let t = gettPS p s in t>=0 && t<=1

-- test if two segments intersect. Doesn't calculate for ix point though.
ixSS :: Autofloat a => LineSeg a -> LineSeg a -> Bool
ixSS (a,b) (c,d) = let
    ncd = rot90 $ d `sub` c
    a_cd = ncd `dot` (a `sub` c)
    b_cd = ncd `dot` (b `sub` c)
    nab = rot90 $ b `sub` a
    c_ab = nab `dot` (c `sub` a)
    d_ab = nab `dot` (d `sub` a)
    in ((a_cd>=0&&b_cd<=0) || (a_cd<=0&&b_cd>=0)) && 
       ((c_ab>=0&&d_ab<=0) || (c_ab<=0&&d_ab>=0))

-- sample points along segment with interval.
sampleS :: (Autofloat a, Enum a) => a -> LineSeg a -> [Point a]
sampleS interval (a, b) = let
    l = lenS (a, b)
    numSamples = floor $ l/interval
    inds = map realToFrac [0..numSamples-1]
    ks = map (/(realToFrac numSamples)) $ inds
    in map (lerp a b) ks

------- polygon ----------

-- represents a polygon without holes as list of vertices
type Blob a = [Point a]

getSegmentsB :: Blob a -> [LineSeg a]
getSegmentsB pts = let 
    lastInd = length pts - 1
    f x = if x==lastInd then (pts!!lastInd, pts!!0) else (pts!!x, pts!!(x+1))
    in map f [0..lastInd]

moveBlob :: Autofloat a => Blob a -> Vect a -> Blob a
moveBlob blob m = map (`sub`m) blob

-- returns True if point inside the blob
isInB :: Autofloat a => Blob a -> Point a -> Bool
isInB pts [x0,y0] = let
    diffp = map (\[x,y]->[x-x0,y-y0]) pts
    getAngle [x,y] = atan2 y x 
    sweeps = map (\(p1,p2)->(getAngle p2)-(getAngle p1)) (getSegmentsB diffp)
    adjust sweep = 
        if sweep>pi then sweep-2*pi
        else if sweep<(-pi) then 2*pi+sweep
        else sweep
    sweepAdjusted = map adjust sweeps
    -- if inside pos poly, res would be 2*pi,
    -- if inside neg poly, res would be -2*pi,
    -- else res would be 0
    res = foldl (+) 0.0 sweepAdjusted
    in res>pi || res<(-pi) 

type Polygon a = (Blob a, [Blob a])

getVerticesG :: Polygon a -> [Point a]
getVerticesG (bds, hs) = concat [bds, concat hs]

getSegmentsG :: Polygon a -> [LineSeg a]
getSegmentsG (bds, hs) = let
    bdsegments = getSegmentsB bds
    hsegments = map (\h->getSegmentsB h) hs
    in concat [bdsegments, concat hsegments]

sampleG :: (Autofloat a, Enum a) => a -> Polygon a -> [Point a]
sampleG interval poly = concat $ map (sampleS interval) $ getSegmentsG poly

------ TODO: other shapes ----
type Polyline a = [Point a]
type Ellipse a = (Point a, a, a)
type Rectangle a = (Point a, Point a)

--------- queries on shapes -----------

-- distance squared
-- could further optimize on code level
dsq :: Autofloat a => Shape a -> Shape a -> a
dsq (Pt p1) (Pt p2) = magsq $ p2 `sub` p1
dsq (Seg (a,b)) (Pt p) = let t = gettPS p (a,b) in
    if t<0 then dsq (Pt a) (Pt p)
    else if t>1 then dsq (Pt b) (Pt p)
    else (**2) $ (normS (a,b)) `dot` (p `sub` a)
dsq (Seg (a,b)) (Seg (c,d)) = if ixSS (a,b) (c,d) then 0 else let
    da = dsq (Seg (c,d)) (Pt a)
    db = dsq (Seg (c,d)) (Pt b)
    dc = dsq (Seg (a,b)) (Pt c)
    dd = dsq (Seg (a,b)) (Pt d)
    in min (min da db) (min dc dd)
dsq (Poly g) (Pt p) = let 
    segments = getSegmentsG g
    d2segments = map (\s -> dsq (Seg s) (Pt p)) segments
    in foldl min posInf d2segments
dsq (Poly g) (Seg s) = foldl min posInf $ map (\e->dsq(Seg e)(Seg s)) $ getSegmentsG g
dsq (Poly g1) (Poly g2) = let
    min1 = foldl min posInf $ map (\e -> dsq (Poly g2) (Seg e)) $ getSegmentsG g1
    min2 = foldl min posInf $ map (\e -> dsq (Poly g1) (Seg e)) $ getSegmentsG g2
    in min min1 min2

-- distance
-- could be slightly more efficient if don't all reduce to dsq...
dist :: Autofloat a => Shape a -> Shape a -> a
dist shape1 shape2 = (**0.5) $ dsq shape1 shape2

isIn :: Autofloat a => Shape a -> Point a -> Bool
isIn (Poly (bds, hs)) p = if (dsq(Poly(bds,hs))(Pt p)) < eps then True else let
    inh = foldl (||) False $ map (\h->isInB h p) hs
    in (isInB bds p) && (not inh)
isIn _ p = False

----------------------- other random tests -----------------------------

energy p [x,y] = dsq p $ Pt[x,y]

energy2 p [x,y] = dsq (Poly([[3.0,0.0], [0.0,3.0], [x,y]],[])) $ p

density = 0.1

-- seems to work with AD just fine. Although manual version takes only ~63% of run time.
-- energy of B inside A. trans is for B.
dsqIntegralIn :: (Autofloat a, Enum a) => Shape a -> Shape a -> [a] -> a
dsqIntegralIn shapeA (Poly polyB) transf = let
    samples = sampleG density polyB
    f p = isIn shapeA $ transformP p transf
    samplesIn = filter f samples
    in (*density) $ foldl (+) 0.0 $ map (\p -> dsq shapeA $ Pt(transformP p transf)) samplesIn

-- energy of B outside A. trans is for B.
dsqIntegralOut :: (Autofloat a, Enum a) => Shape a -> Shape a -> [a] -> a
dsqIntegralOut shapeA (Poly polyB) transf = let
    samples = sampleG density polyB
    f p = not $ isIn shapeA $ transformP p transf
    samplesOut = filter f samples
    in (*density) $ foldl (+) 0.0 $ map (\p -> dsq shapeA $ Pt(transformP p transf)) samplesOut

-- containment energy, how energy's calculated could stay the same (I think),
-- but might want to change type signature 
eAcontainB :: (Autofloat a, Enum a) => Shape a -> Shape a -> [a] -> a
eAcontainB shapeA shapeB [x1,x2,x3,x4,x5,x6,x7,x8] = let
    eAinB = dsqIntegralIn shapeB shapeA [x1,x2,x3,x4]
    eBoutA = dsqIntegralOut shapeA shapeB [x5,x6,x7,x8]
    in eAinB + eBoutA

-- this one works w AD, run: test (dsqEnergy (Pt[0.0,0.0]) (Pt[0.0,0.0])) [1,1,0,1]
dsqEnergy :: (Autofloat a, Enum a) => Shape a -> Shape a -> [a] -> a
dsqEnergy shapeA (Pt p2) transf = let
    shapeB = Pt (transformP p2 transf)
    in dsq shapeA shapeB

testAD :: (Autofloat a, Enum a) => (forall b. (Autofloat b, Enum b) => [b] -> b) -> [a] -> [a]
testAD f arg = grad f arg

runTests count = let
    res = foldl add [0,0,0,0] $ 
        map (\x->testAD (dsqIntegralOut polyA polyB) [-3.5,2.9,0.03,1]) [1..count-1]
    in trace ((show res)++"\nignore above. true result is: ") $ 
    testAD (dsqIntegralOut polyA polyB) [-3.5,2.9,0.03,1]

polyA = Poly([[206.0,300.0],[179.0,420.0],[298.0,484.0],[333.0,398.0],[419.0,417.0],[423.0,305.0]],[])
polyB = Poly([[465.0,163.0],[488.0,230.0],[584.0,257.0],[586.0,313.0],[674.0,283.0],[628.0,163.0]],[])

g = Poly ([[0,0],[1,0],[1,1],[0,1]],[])
h = Poly ([[0.5,0.5],[-0.5,0.5],[-0.5,-0.5],[0.5,-0.5]], [])
a = fromShape g

p = Pt [2,3]
s = Seg([1,2],[3.2,4])

doub :: Double
doub = 3.456

af :: Autofloat a => a
af = realToFrac doub

str = show g
rd = read str :: Shape Double

s1 = ([-2,0],[2,2])
s2 = ([0,0],[-1,1])
res = trace (show $ ixSS s1 s2) 0

echo :: Autofloat a => [ShapeNode a] -> [ShapeNode a]
echo state = state
