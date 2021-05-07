module Problems.P025
    ( p025
    ) where

import EulerMath

{- fib n >= 1e999
 - let phi = (1 + sqrt 5) / 2
 - 1e999 <= (phi^n - (1 - phi)^n) / sqrt 5
 - (1 - phi)^n -> 0 as n -> inf, so we can ignore it for large n (I think that 1e999 qualifies as a large number)
 - 1e999 * sqrt 5 <= phi^n
 - logBase phi 1e999 + logBase phi (sqrt 5) <= n
 - 4781.859270753069 <= n
 - round down to int: 4781
 - 4781 -> 4782 because Project Euler uses 1-based indexing
 -}

p025 :: IO ()
p025 = print 4781
