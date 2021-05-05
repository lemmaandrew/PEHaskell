module Problems.P005
    ( p005
    ) where

{- lcm Reduction:
 - The lcm of a number and its factors is that number
 - So we can remove all the factors of a number < the number
 - 20: (1,20), (2,10), (4,5) -> [1,2,4,5,10]
 - 19: (1,19) -> [1]
 - 18: (1,18), (2,9), (3,6) -> [1,2,3,6,9]
 - 17: (1,17) -> [1]
 - 16: (1,16), (2,8), (4,4) -> [1,2,4,8]
 - 15: (1,15), (3,5) -> [1,3,5]
 - ...
 - removed: [1 .. 10]
 - remaining: [11 .. 20]
 - -}

p005 :: IO ()
p005 = print $ foldr1 lcm [11 .. 20]
