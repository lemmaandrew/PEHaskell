module Problems.P020
    ( p020
    ) where

import EulerMath ( sum' )

p020 :: IO ()
p020 = print $ sum' [fromEnum x - 48 | x <- show $ product [2 .. 100]]
