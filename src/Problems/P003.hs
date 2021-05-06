module Problems.P003
    ( p003
    ) where

import EulerMath ( primeFactorsTMWE )

p003 :: IO ()
p003 = print $ last $ primeFactorsTMWE 600851475143
