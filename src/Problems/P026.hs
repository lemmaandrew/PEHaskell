module Problems.P026
    ( p026
    ) where

import Data.IntSet ( IntSet, insert, member, size )
import Data.List   ( maximumBy )
import Data.Ord    ( comparing )

getCycleLen :: Int -> Int
getCycleLen n = size $ go mempty (10 ^ length (show n))
  where
    go rems dividend =
        let r = dividend `mod` n
        in if r `member` rems
                then rems
                else go (insert r rems) (r * 10)

p026 :: IO ()
p026 = print $ maximumBy (comparing getCycleLen) [1 .. 1000]
