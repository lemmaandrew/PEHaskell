module Problems.P029
    ( p029
    ) where

import Data.Set ( Set, fromList )

-- I could do an combinatorics approach.
-- But brute forcing is such a non-issue
-- that deriving a solution is simply not worth the issue.
solve :: Int
solve = length $ fromList [a^b | a <- [2 .. 100], b <- [2 .. 100]]


p029 :: IO ()
p029 = print solve
