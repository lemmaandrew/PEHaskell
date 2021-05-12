module Problems.P033
    ( p033
    ) where

{-

Let our fraction take the form of:
    ac / bc
where:
    a, b, c are integers <= 9
    c is the digit to be cancelled

Thus our numerator `numer`   = 10*a + c or 10*c + a
         denominator `denom` = 10*b + c or 10*c + b

Because numer must be less than denom, a < b

For our fraction to not be trivial or single-digitin the numerator and/or denominator:
    a, b, c /= 0

This makes our bounds:
    1 <= a < b <= 9
    1 <= c <= 9

Our resulting cancelable equations are:
    (10*c + a) / (10*c + b) = a / b  -- cancel first digit
    (10*a + c) / (10*b + c) = a / b  -- cancel second digit
    (10*c + a) / (10*b + c) = a / b  -- cancel first and second digits
    (10*a + c) / (10*c + b) = a / b  -- cancel second and first digits

-}
import Data.List  ( nub )
import Data.Ratio

-- | Gets the cancelable fractions from digits a, b, and c
cancelableFracs :: Integral a => a -> a -> a -> [Ratio a]
cancelableFracs a b c =
    filter
        isCancelable
        [ (10 * c + a) % (10 * c + b)
        , (10 * a + c) % (10 * b + c)
        , (10 * c + a) % (10 * b + c)
        , (10 * a + c) % (10 * c + b)
        ]
  where
    ab = a % b
    isCancelable rat = rat == ab

solve :: Integral a => Ratio a
solve =
    product $
    nub $
    concat [cancelableFracs a b c | b <- [2 .. 9], a <- [1 .. b - 1], c <- [1 .. 9]]

p033 :: IO ()
p033 = print $ denominator solve
