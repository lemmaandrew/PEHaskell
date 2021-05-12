module Problems.P031
    ( p031
    ) where

import EulerMath ( sum' )

solve :: (Integral a, Num b) => [a] -> a -> b
solve [_] _ = 1
solve coins target = sum' $ map go [0 .. target `div` head coins]
  where
    go coin = solve (tail coins) (target - coin * head coins)

p031 :: IO ()
p031 =
    let coins = reverse [1, 2, 5, 10, 20, 50, 100, 200]
    in print $ solve coins 200
