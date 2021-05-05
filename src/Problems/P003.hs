module Problems.P003
    ( p003
    ) where

import EulerMath ( primeFactors )

p003 :: IO ()
p003 = print $ last $ primeFactors 600851475143
