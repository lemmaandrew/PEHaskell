module Problems.P021
    ( p021
    ) where

import           Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import EulerMath ( sum', factors, sumFactors )

amicable :: Integral a => a -> Maybe a
amicable 1 = Nothing
amicable n =
    let sd = sumFactors n
    in if sumFactors sd == n && sd /= n
            then Just sd
            else Nothing

solve :: Int
solve = go IS.empty [1 .. 9999]
  where
    go is [] = IS.foldl' (+) 0 is
    go is (x:xs)
        | IS.member x is = go is xs
        | otherwise =
            case amicable x of
                Just y ->
                    let 
                        pushX = IS.insert x
                        pushY = if y <= 9999 then IS.insert y else id
                    in go (pushY $ pushX is) xs
                _ -> go is xs

p021 :: IO ()
p021 = print solve
