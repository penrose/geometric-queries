module Linesearch (
  linesearch,
  linesearch'
) where 

import Debug.Trace

epsilon = 0.1 ** 30
betaInit = 1/0
(k1, k2) = (0.05, 0.95)
initialt = 0.12 -- avoids cumulative scale 0 (at which grad is not defined)

maxLoop = 120

-- input function to optimize and its gradient
linesearch' :: ([Double] -> Double) -> ([Double] -> [Double]) -> [Double] -> [Double] -> Maybe [Double] -- returns t
linesearch' f g dir arg = trace (show arg) $ let
  --dir = neg $ normalize $ g arg --search direction: opposite to gradient (should not have to swap ever?)
  in trace (show$"dir: "++(show dir)) $ if f arg < epsilon then trace ("alr satisfied. ") Nothing -- TODO : this check shouldn't be in linesearch
  else if (directional g dir arg) > 0 then let -- swap at x = arg
  -- if grad is positive, swap the function
  res = trace "flip" $ linesearch' f g (neg dir) arg
  in case res of Nothing -> Nothing
                 Just x -> Just (neg x)
  else let
  -- loop part
  f0 = f arg --Double
  g0 = directional g dir arg --Double
  searchLoop alpha beta t counter = if counter > maxLoop then trace "reached max loop count" Nothing
  else let 
    f1 = f (add arg $ mult dir t) -- Double, f [moved in dir of grad of arg by t amt]
    g1 = directional g dir (add arg $ mult dir t) -- Double, g [...]
    (alpha', beta') = let am = armijo f0 f1 g0 k1 t in
      if (not am && g1>0) then (alpha, t)
      else if wolfeR g0 g1 k2 then (alpha, t) -- overshot while growing interval, start shrinking
      else if wolfeL g0 g1 k2 then (t, beta) 
      else (t, t)
    in trace ("range: "++(show alpha')++", "++(show beta')) $ if beta'-alpha' < epsilon then trace 
         ("finished in steps: "++(show counter)++"\nresult step: "++(show alpha')) 
         Just $ mult dir alpha' --end condition: range small enough, returns stepsize (how much should displace from arg)
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

-- input function to optimize and its gradient
linesearch :: (Double -> Double) -> (Double -> Double) -> Double -> Maybe Double
linesearch f g arg = if f arg < epsilon then trace ("alr satisfied. ") Nothing -- TODO : this check shouldn't be in linesearch
  else if g arg > 0 then let -- swap at x = arg
  f' x = f (2*arg-x)
  g' x = - (g (2*arg-x))
  -- if grad is positive, swap the function
  res = linesearch f' g' arg
  in case res of Nothing -> Nothing
                 Just x -> Just (-x)
  else let
  -- loop part
  f0 = f arg
  g0 = g arg
  searchLoop alpha beta t counter = if counter > maxLoop then trace "reached max loop count" Nothing
  else let 
    f1 = f (arg+t)
    g1 = g (arg+t)
    (alpha', beta') = let am = armijo f0 f1 g0 k1 t in
      if (not am && g1>0) || wolfeR g0 g1 k2 then (alpha, t) -- overshot while growing interval, start shrinking
      else if wolfeL g0 g1 k2 then (t, beta) 
      else (t, t)
    in trace ("range: "++(show alpha')++", "++(show beta')) $ if beta'-alpha' < epsilon then trace 
         ("finished in steps: "++(show counter)++"\nresult step: "++(show alpha')) 
         Just alpha' --end condition: range small enough, returns stepsize (how much should displace from arg)
       else if beta' < betaInit then searchLoop alpha' beta' ((beta'+alpha')/2) (counter+1)
       else searchLoop alpha' beta' (alpha' * 2) (counter+1)
  in searchLoop 0 betaInit initialt 0

armijo f0 f1 g0 k1 t = f1 <= f0 + k1*t*g0*f0 

wolfeL g0 g1 k2 = g1 < 0 && (abs g1) >= (abs $ g0*k2)

wolfeR g0 g1 k2 = g1 >= 0 && (abs g1) >= (abs $ g0*k2)
