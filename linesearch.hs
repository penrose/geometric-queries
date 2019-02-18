module Linesearch (
  linesearch
) where 

import Debug.Trace

epsilon = 0.1 ** 10
betaInit = 1/0
(k1, k2) = (0.5, 0.96)
initialt = 0.3 -- avoids cumulative scale 0 (at which grad is not defined)

maxLoop = 120

-- input function to optimize and its gradient. returns t
linesearch :: ([Double] -> Double) -> ([Double] -> [Double]) -> 
  [Double] -> [Double] -> Maybe [Double]
linesearch f g dir arg = let
  in trace (show$"dir: "++(show dir)) $ 
    if trace "calculating energy when entering linesearch..." $ f arg < epsilon 
    then trace ("alr satisfied. ") Nothing
  else if (directional g dir arg) > 0 then let -- swap at x = arg
  -- if grad is positive, swap the function
  res = trace "flip" $ linesearch f g (neg dir) arg
  in case res of Nothing -> Nothing
                 Just x -> Just (neg x)
  else let
  -- loop part
  f0 = f arg --Double
  g0 = directional g dir arg --Double, directional derivative
  searchLoop alpha beta t counter = if counter > maxLoop then trace "reached max loop count" Nothing
  else let 
    f1 = f (add arg $ mult dir t) -- Double, f [moved in dir of grad of arg by t amt]
    gDir = directional g dir (add arg $ mult dir t)
    (alpha', beta') = 
      let am = trace ("judging for armijo: f0: "++(show f0)++" f1: "++(show f1)) $ armijo f0 f1 g0 k1 t
      in if trace ("armijo: "++(show am)) $ (not am) then (alpha, t)
      else if wolfeR g0 gDir k2 then (alpha, t) -- overshot while growing interval, start shrinking
      else if wolfeL g0 gDir k2 then (t, beta) 
      else (t, t)
    in trace ("range: "++(show alpha')++", "++(show beta')) $ if beta'-alpha' < epsilon then trace 
         ("finished in steps: "++(show counter)++"\nresult step: "++(show alpha')++
          "\n------input dir grad: "++(show$g0)++"\n------output dir grad: "++(show$gDir))
         Just $ mult dir alpha' 
         --end condition: range small enough, returns stepsize (how much should displace from arg)
       else if beta' < betaInit then searchLoop alpha' beta' ((beta'+alpha')/2) (counter+1)
       else searchLoop alpha' beta' (alpha' * 2) (counter+1)
  in searchLoop 0 betaInit initialt 0

mag :: [Double] -> Double
mag xs = sqrt $ foldl (\a b->a+b) 0 (map (\x->x**2) xs)

directional :: ([Double] -> [Double]) -> [Double] -> [Double] -> Double
directional g dir arg = let 
  res = dot dir $ g arg
  in res

dot :: [Double] -> [Double] -> Double
dot v1 v2 = foldl (\a b->a+b) 0 $ map (\(a,b)->a*b) (zip v1 v2)

normalize :: [Double] -> [Double]
normalize xs = let magnitude = mag xs in
  map (\x -> x/magnitude) xs

add :: [Double] -> [Double] -> [Double]
add v1 v2 = map (\(a,b)->a+b) (zip v1 v2)

minus :: [Double] -> [Double] -> [Double]
minus v1 v2 = map (\(a,b)->a-b) (zip v1 v2)

mult :: [Double] -> Double -> [Double]
mult v a = map (\x -> a*x) v

neg :: [Double] -> [Double]
neg v = map (\x->(-x)) v

armijo f0 f1 g0 k1 t = f1 <= f0 + k1*t*g0

wolfeL g0 g1 k2 = g1 < 0 && (abs g1) >= (abs $ g0*k2)

wolfeR g0 g1 k2 = g1 >= 0 && (abs g1) >= (abs $ g0*k2)
