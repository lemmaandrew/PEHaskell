module Problems.P007
    ( p007
    ) where

import EulerMath ( primesSA )

p007 :: IO ()
p007 = print $ primesSA !! 10000
