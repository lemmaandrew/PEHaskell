module Problems.P036
    ( p036
    ) where

{-

Because the leading digit of a binary number is always 1,
    for the number to be palindromic, the last digit also must be 1.
Therefore, the number must be odd.

 -}


import Data.Char
import EulerMath
import Numeric

isPalindrome :: Show a => a -> Bool
isPalindrome x = reverse sx == sx
  where
    sx = show x

toBin :: (Integral a, Show a) => a -> String
toBin n = showIntAtBase 2 intToDigit n ""

solve :: (Integral a, Show a) => a
solve = sum' [x | x <- [1, 3 .. 1000000], isPalindrome x && isPalindrome (toBin x)]

p036 :: IO ()
p036 = print solve
