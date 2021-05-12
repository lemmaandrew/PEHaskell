{-# LANGUAGE OverloadedLists #-}

module Problems.P034
    ( p034
    ) where

{-
Similar to Problem 30, upper bound is determined by n * f(9) for some function f
Here, f = factorial
We have 7 * 9! == 2540160, which is 7 digits.
8 * 9! == 2903040 is also 7 digits,
    so the growth of the number becomes larger than the growth of its digital sum at this point.
    Therefore we can stop at 7 * 9!

I can't think of a faster way to compute this other than a faster factorial function.
I tried precomputing factorials 0 - 9 and storing them into a Vector and accessing them,
    but somehow this was slower than just doing the factorial.
 -}
import Data.Char ( digitToInt )
import Data.Vector ( Vector, (!) )
import EulerMath ( factorial, sum' )

digitFactSum :: Show a => a -> Int
digitFactSum n = sum' [factorial (digitToInt x) | x <- show n]

solve :: Int
solve = sum' [dfs | x <- [3 .. 2540160], let dfs = digitFactSum x, dfs == x]

p034 :: IO ()
p034 = print solve
