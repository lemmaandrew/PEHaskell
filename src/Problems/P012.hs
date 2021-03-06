module Problems.P012
    ( p012
    ) where

import EulerMath ( numFactorsSA )

triangleNums :: (Enum a, Num a) => [a]
triangleNums = scanl1 (+) [1..]

p012 :: IO ()
p012 = print $ head $ dropWhile ((<= 500) . numFactorsSA) triangleNums
