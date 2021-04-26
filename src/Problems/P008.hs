module Problems.P008
    ( p008
    ) where

import Data.List
import EulerMath

loadData :: IO [Int]
loadData = do
    fileStr <- filter (/= '\n') <$> readFile "/home/peter/programs/ProjectEuler/PEHaskell/problem_inputs/P008.txt"
    return $ map (read . return) fileStr

solve :: IO Int
solve = do
    data' <- loadData
    return $ maximum [product $ take 13 xs | xs <- tails data', lazyLen xs > 12]

p008 :: IO ()
p008 = solve >>= print
