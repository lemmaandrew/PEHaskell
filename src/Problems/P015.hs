module Problems.P015
    ( p015
    ) where

choose :: Integral a => a -> a -> a
choose _ 0 = 1
choose 0 _ = 0
choose n k = choose (n - 1) (k - 1) * n `div` k

p015 :: IO ()
p015 = print $ choose 40 20
