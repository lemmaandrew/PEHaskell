module Problems.P010
    ( p010
    ) where

import EulerMath ( sum', primesSA )

p010 :: IO ()
p010 =
    let ps = takeWhile (< 2000000) primesSA
    in print $ sum' ps
