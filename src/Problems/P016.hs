module Problems.P016
    ( p016
    ) where

p016 :: IO ()
p016 = print $ sum [fromEnum c - 48 | c <- show $ 2^1000]
