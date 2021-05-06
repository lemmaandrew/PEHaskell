module Problems.P010
    ( p010
    ) where

import EulerMath ( sum', primesTMWE )

p010 :: IO ()
p010 =
    let ps = takeWhile (< 2000000) primesTMWE
    in print $ sum' ps
