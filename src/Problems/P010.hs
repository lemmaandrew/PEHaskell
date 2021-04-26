module Problems.P010
    ( p010
    ) where

import EulerMath ( sum', primes )

p010 :: IO ()
p010 =
    let ps = takeWhile (< 2000000) primes
    in print $ sum' ps
