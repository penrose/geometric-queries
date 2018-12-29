module Linesearch (
  linesearch,
  -- belor for debug
  armijo,
  wolfeL,
  wolfeR
) where 

import Debug.Trace

epsilon = 0.1 ** 30
betaInit = 1/0
(k1, k2) = (0.1**4, 0.999)
initialt = 0.1

maxLoop = 120

-- input function to optimize and its gradient
linesearch :: (Double -> Double) -> (Double -> Double) -> Double -> Maybe Double
linesearch f g arg = if f arg < epsilon then Nothing -- TODO : this check shouldn't be in linesearch
  else if g arg > 0 then trace "swap!" $ let -- swap at x = arg
  f' x = f (2*arg-x)
  g' x = - (g (2*arg-x))
  -- if grad is positive, swap the function
  res = linesearch f' g' arg
  in case res of Nothing -> Nothing
                 Just x -> Just (-x)
  else let
  -- loop part
  searchLoop alpha beta t counter = if counter > maxLoop then trace "nothing" Nothing
  else let 
    f0 = f arg
    g0 = g arg
    f1 = f (arg+t)
    g1 = g (arg+t)
    (alpha', beta') = 
      if (not (armijo f0 f1 g0 k1 t)) ||
         (wolfeR g0 g1 k2)
      then (alpha, t) else
      if wolfeL g0 g1 k2
      then (t, beta) else (t, t)
    in if beta'-alpha' < epsilon then trace 
         ("finished in steps: "++(show counter)++"\nresult step: "++(show alpha')) 
         Just alpha' --end condition: range small enough
       else if beta' < betaInit then searchLoop alpha' beta' ((beta'+alpha')/2) (counter+1)
       else searchLoop alpha' beta' (alpha' * 2) (counter+1)
  in searchLoop 0 betaInit initialt 0

armijo f0 f1 g0 k1 t = f1 <= f0 + k1*t*g0

wolfeL g0 g1 k2 = g1 < 0 && (abs g1) >= (abs $ g0*k2)

wolfeR g0 g1 k2 = g1 >= 0 && (abs g1) >= (abs $ g0*k2)
