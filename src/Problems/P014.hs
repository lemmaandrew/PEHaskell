module Problems.P014
    ( p014
    ) where

import           Data.IntMap        ( IntMap, (!?) )
import qualified Data.IntMap.Strict as IM

collatz :: Int -> IntMap Int -> (Int, IntMap Int)
collatz 1 im = (0, im)
collatz n im =
    case im !? n of
        Just x -> (x, im)
        _ ->
            let n' =
                    if even n
                        then n `div` 2
                        else 3 * n + 1
                (x, im') = collatz n' im
            in (x + 1, IM.insert n (x + 1) im')

solve :: Int
solve = go 0 0 IM.empty [1 .. 999999]
  where
    go maxKey _ _ [] = maxKey
    go maxKey maxVal im (x:xs) =
        let (v, im') = collatz x im
        in if v > maxVal
                then go x v im' xs
                else go maxKey maxVal im' xs

p014 :: IO ()
p014 = print solve
