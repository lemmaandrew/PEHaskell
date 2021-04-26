module Problems.P002
    ( p002
    ) where

import EulerMath ( sum', fibs )

p002 :: IO ()
p002 =
    let terms = takeWhile (<= 4000000) $ filter even fibs
    in print $ sum' terms
