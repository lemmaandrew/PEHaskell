module Problems.P007
    ( p007
    ) where

import EulerMath ( primesTMWE )

p007 :: IO ()
p007 = print $ primesTMWE !! 10000
