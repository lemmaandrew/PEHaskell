module Problems.P032
    ( p032
    ) where

{-
Let a, b, c be integers
Let pid = product identity (show a ++ show b ++ show c)
Because pid must be pandigital, length pid == 9

For pid to be nine digits, the max length of any term is 4:
    a = 1
    b = num len 4
    c = num len 4
    a * b = c -> len1 * len4 = len4 -> 1 + 4 + 4 = 9 digits

If we let a always be < b (which we should as to avoid double calculations), then we have:
    If a < 10 then b == 4 digits else b == 3 digits
    Furthermore, because max len c is 4 digits, then max c = 9999, and b = c / a

Decided to use a makeshift bitset because of a coding assignment a friend had to do
    and I wanted to emulate them lol
 -}
import Data.Bits ( Bits((.|.), shiftL, (.&.)) )
import Data.Char ( digitToInt )
import Data.List ( nub )
import EulerMath ( sum' )

isPanDigital :: Int -> Bool
isPanDigital = go 0
  where
    go :: Int -> Int -> Bool
    go bitSet 0 = bitSet == 511
    go bitSet n =
        let (d, m) = divMod n 10
            shiftM = 1 `shiftL` (m - 1)
        in m /= 0 && (bitSet .&. shiftM) == 0 && go (bitSet .|. shiftM) d

solve :: Int
solve =
    sum' $
    nub [ p
        | a <- [2 .. 99]
        , b <- [10 ^ (3 - fromEnum (a > 9)) .. 9999 `div` a]
        , let p = a * b
        , isPanDigital $ read $ show a ++ show b ++ show p
        ]

p032 :: IO ()
p032 = print solve
