module Problems.P006
    ( p006
    ) where

import EulerMath

sumSquares :: Integral a => a -> a
sumSquares n = (n * (n + 1) * (2 * n + 1)) `div` 6

squareSums :: Integral a => a -> a
squareSums n = sumRange 1 n 1 ^ 2

p006 :: IO ()
p006 = print $ squareSums 100 - sumSquares 100
