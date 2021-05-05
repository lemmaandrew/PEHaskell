module Problems.P008
    ( p008
    ) where

import Data.List ( tails )
import EulerMath ( lazyLen )
import EulerUtil ( getProblemInput )

loadData :: IO [Int]
loadData = do
    fileStr <- filter (/= '\n') <$> getProblemInput 8
    return $ map (read . return) fileStr

solve :: IO Int
solve = do
    data' <- loadData
    return $ maximum [product $ take 13 xs | xs <- tails data', lazyLen xs > 12]

p008 :: IO ()
p008 = solve >>= print
