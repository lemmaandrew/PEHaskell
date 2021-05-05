module Problems.P018
    ( p018
    ) where

import EulerUtil ( getProblemInput )

loadData :: (Num a, Read a) => IO [[a]]
loadData = do
    rows <- reverse . lines <$> getProblemInput 18
    let rowNumbers = [map read $ words xs | xs <- rows]
    return rowNumbers

solve :: (Num a, Ord a) => [[a]] -> a
solve [[x]] = x
solve (xs:ys:xss) =
    let maxes = zipWith max xs (tail xs)
    in solve (zipWith (+) maxes ys : xss)

p018 :: IO ()
p018 = print . solve =<< loadData
