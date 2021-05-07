module Problems.P023
    ( p023
    ) where

import Data.Array.Unboxed
import Data.List          ( sort, tails )
import Debug.Trace ( traceShowId )
import EulerMath

isAbundant :: Int -> Bool
isAbundant n = sumFactorsSA n > n

abundantNums :: [Int]
abundantNums = filter isAbundant [12 ..]

abunSums :: Int -> UArray Int Bool
abunSums limit =
    accumArray
        (\_ _ -> True)
        False
        (24, limit)
        [ (x + y, ())
        | xs@(x:_) <- tails $ takeWhile (<= limit) abundantNums
        , y <- takeWhile (\y -> x + y <= limit) xs
        ]

solve :: Int -> Int
solve limit = sumRange 1 (fromIntegral limit) 1 - sum' bunnies
  where
    isBunnies = assocs $ abunSums limit
    bunnies = map fst $ filter snd isBunnies

p023 :: IO ()
p023 = print $ solve 28123
