module Problems.P007
    ( p007
    ) where

import EulerMath ( primes )

p007 :: IO ()
p007 = print $ primes !! 10000
