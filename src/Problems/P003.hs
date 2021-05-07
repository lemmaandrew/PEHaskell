module Problems.P003
    ( p003
    ) where

import EulerMath ( primeFactorsSA )

p003 :: IO ()
p003 = print $ last $ primeFactorsSA 600851475143
