module Problems.P013
    ( p013
    ) where

import EulerMath ( sum' )
import EulerUtil ( getProblemInput )

p013 :: IO ()
p013 = do
    nums <- map read . lines <$> getProblemInput 13 :: IO [Integer]
    let s = sum' nums
    putStrLn $ take 10 $ show s
