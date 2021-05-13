module Problems.P016
    ( p016
    ) where

import EulerMath ( sumDigits )

p016 :: IO ()
p016 = print $ sumDigits $ 2^1000
