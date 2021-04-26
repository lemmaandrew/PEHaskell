module Problems.P001
    ( p001
    ) where

-- | Sum of an arithmetic progression with n terms starting at a with a step size of d
sumProgression :: Integral a => a -> a -> a -> a
sumProgression n a d = (n * (2 * a + (n - 1) * d)) `div` 2

p001 :: IO ()
p001 =
    let res =
            sumProgression (999 `div` 3) 3 3 + sumProgression (999 `div` 5) 5 5 -
            sumProgression (999 `div` 15) 15 15
    in print res
