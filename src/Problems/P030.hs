module Problems.P030
    ( p030
    ) where

{-

let n = starting number
n >= 10 because n < 10 is not a sum
This gives us a very low lower bound of n >= 10

Let's try to find an upper limit:
9^5 == 59049, which is a 5 digit number
So, our upper bound will be >= 5 digits
5 * 9^5 == 295245 <- upper bound
 -}

import Data.Char ( digitToInt )
import EulerMath ( sum' )

digitPowSum :: (Show a, Integral b, Num c) => a -> b -> c
digitPowSum n p = sum' [fromIntegral (digitToInt x) ^ p | x <- show n]

isDigitPowSum :: (Num a, Show a, Eq a, Integral b) => a -> b -> Bool
isDigitPowSum n p = n == digitPowSum n p

p030 :: IO ()
p030 = print $ sum' [x | x <- [10 .. 5 * 9^5], isDigitPowSum x 5]
