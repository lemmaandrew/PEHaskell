module Problems.P011
    ( p011
    ) where

import           Data.Vector ( (!) )
import qualified Data.Vector as V
import           EulerUtil   ( getProblemInput )

type VMatrix a = V.Vector (V.Vector a)

problemData :: IO (VMatrix Int)
problemData = do
    str <- getProblemInput 11
    return $
        V.fromList
            [ V.map read vwords
            | line <- lines str
            , let vwords = V.fromList $ words line
            ]

solve :: VMatrix Int -> Int
solve xss =
    maximum
        [prodAdj i j | i <- [0 .. length xss - 1], j <- [0 .. length (V.head xss) - 1]]
  where
    prodAdj row col
        | xss ! row ! col == 0 = 0
        | otherwise = maximum [right, down, diagR, diagL]
      where
        right
            | col < length (V.head xss) - 3 =
                product $ V.slice col 4 (xss ! row)
            | otherwise = 0
        down
            | row < length xss - 3 =
                product [xss ! (row + i) ! col | i <- [0 .. 3]]
            | otherwise = 0
        diagR
            | row < length xss - 3 && col < length (V.head xss) - 3 =
                product [xss ! (row + i) ! (col + i) | i <- [0 .. 3]]
            | otherwise = 0
        diagL
            | row < length xss - 3 && col >= 3 =
                product [xss ! (row + i) ! (col - i) | i <- [0 .. 3]]
            | otherwise = 0

p011 :: IO ()
p011 = do
    data' <- problemData
    print $ solve data'
