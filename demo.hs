{- To compile: 
 -   $ ghci
 -   ghci> :l <filename>.hs
 -   (after first time load)> :r
 - Or
 -   Have a function called main
 -   $ ghc --make <filename>
 -   $ ./<filename>
 -}

-- basics --
r = 5.0 --float
area = pi * r^2
name = "miyehn" --string, really an array of chars ['m','i','y'...]
False = 5 /= 5
{- About types:
 - capitalized word is an explicit type.
 - any other letter(s) is type variable (polymorphism)
 - [Int] means Int list
 - Anything appears before => is class constraint. Ex: Eq a
 -      first word of which is its typeclass (Eq)
 -      Other typeclasses: Ord, Show, Read (read is a useful func),
 -        Enum (succ, pred), Num (contains Int, Integer, Float, Double),
 -        Integral, Floating
 -
 - Can import modules by putting:
 - import (qualified) <moduleName> (as <alias>)
 -}

-- functions --
doub x = x + x
add x y = x + y
isOdd x = if x `mod` 2 == 0 then False else True
-- function names can't begin with capital letters
main = putStrLn "Hello World" -- main function; print

notOne (1, a) = a
notOne (a, 1) = a -- still defining func

fib :: Int -> Int -- type annotation. To print out type, ghci>:t <exp>
fib x = case x of -- casing, pattern matching
  0 -> 0
  1 -> 1
  x -> fib (x-1) + fib (x-2)

cmp x y -- guards
  | x > y = GT
  | x < y = LT
  | otherwise = EQ -- otherwise == True

dist (a,b) (c,d) = let -- let-in block, "where" looks similar
  sqdx = (c - a)^2
  sqdy = (d - b)^2
  in sqrt(sqdx + sqdy)

-- lists --
l1 = [1,2,3,4]
l2 = [5,6]
l3 = l1 ++ l2 -- concat, O(n)
l4 = 0:l3 -- cons, O(1)
l5 = [[1,2],[3,4],[5,6]]
l6 = ['a'..'z'] -- Texas ranges
l7 = [20,18..0] -- Texas range specified with step.

l8 = [1,2..] -- an infinite list (lazy)
l9 = take 9 (cycle [1,2,3]) -- also an infinite list (lazy)

a = l5!!1!!0 -- use !! to access elem by index

True = [1,2] == [1,2] -- can test equality of lists, lexicographically
False = null [1] -- checks if a list is empty
-- Other functions on lists: take, drop, maximum, minimum, sum, product, elem (which tells if an elem is in the list)

m = "miyehn"!!0

h = head l4 -- first elem
t = tail l4 -- all but first elems
l = last l4 -- last elem
i = init l4 -- all but last elem
len = length l4

-- Next time continue at 1.4 on ref card --
