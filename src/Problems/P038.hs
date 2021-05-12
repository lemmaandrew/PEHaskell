module Problems.P038
    ( p038
    ) where

import Data.Bits ( Bits (shiftL, (.&.), (.|.)) )

isPanDigital :: Int -> Bool
isPanDigital = go 0
  where
    go :: Int -> Int -> Bool
    go bitSet 0 = bitSet == 511
    go bitSet n =
        let (d, m) = divMod n 10
            shiftM = 1 `shiftL` (m - 1)
        in m /= 0 && (bitSet .&. shiftM) == 0 && go (bitSet .|. shiftM) d

-- | Gets the largest product sum of a number less than 10 digits
getProdSum :: (Num a, Show a, Read b) => a -> b
getProdSum n = go 0 "" 1
  where
    go digits pstr x =
        let p = n * x
            ps = show p
            len = length ps
        in if digits + len > 9
                then read pstr
                else go (digits + len) (pstr ++ ps) (x + 1)

solve :: Int
solve =
    maximum
        [ prodSum
        | n <- [1 .. 9999] -- can't be 5 digits because there must be more than one term
        , let prodSum = getProdSum n
        , isPanDigital prodSum
        ]

p038 :: IO ()
p038 = print solve
