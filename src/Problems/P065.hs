module Problems.P065
    ( p065
    ) where

import Data.List ( inits )
import Data.Ratio ( numerator )
import EulerMath ( sumDigits )

eTerms :: (Enum a, Num a) => [a]
eTerms = 2 : 1 : 2 : ([4,6 ..] >>= \x -> [1, 1, x])

contFrac :: (Foldable t, Fractional a) => t a -> a
contFrac = foldr1 (\a b -> a + 1 / b)

eConts :: [Rational]
eConts = map contFrac $ tail $ inits eTerms

p065 :: IO ()
p065 = print $ sumDigits (numerator $ eConts !! 99)
