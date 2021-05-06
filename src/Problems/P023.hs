module Problems.P023
    ( p023
    ) where

import           Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import           Data.List   ( sort, tails )
import           EulerMath

isAbundant :: Integral a => a -> Bool
isAbundant n = sumFactorsTMWE n > n

abundantNums :: Integral a => [a]
abundantNums = filter isAbundant [12 ..]

abunSums limit =
    [ x + y
    | (x:xs) <- tails $ takeWhile (< limit) abundantNums
    , y <- takeWhile (\y -> x + y < limit) xs
    ]

solve =
    let limit = 28123
    in sum' $ [1 .. limit] `minus` sort (abunSums limit)

p023 :: IO ()
p023 = print $ takeWhile (<= 28123) abundantNums -- solve
