module Problems.P020
    ( p020
    ) where

import EulerMath ( factorial, sumDigits )

p020 :: IO ()
p020 = print $ sumDigits $ factorial 100
