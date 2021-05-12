module Problems.P037
    ( p037
    ) where

{-
For right-to-left truncation, this is basically just the same problem as Problem 35:
    Just now we check if 0, 2, 4, 5, 6, or 8 is an element of the tail of prime.
For left-to-right truncation, however, we have to brute force it :/

Alternatively!!!! We can generate left-to-right primes and right-to-left primes.
But this has the downside of having to check each generated number for primality.

-}
import Data.List ( sort )
import EulerMath ( intersect, isPrime, sum' )

-- | A small list (length 79), generation is very fast (because only 5 appended digits instead of 9)
-- So we can use the upper bound of this as the upper bound for left-right trunc primes
rightLeftTruncPrimes :: Integral a => [a]
rightLeftTruncPrimes = concatMap gen [2, 3, 5, 7]
  where
    gen p =
        let ps =
                [ nextP
                | x <- [1, 3, 5, 7, 9]
                , let nextP = p * 10 + x
                , isPrime nextP
                ]
        in ps ++ concatMap gen ps

leftRightTruncPrimes :: Integral b => b -> [b]
leftRightTruncPrimes limit = concatMap (gen 10) [2, 3, 5, 7]
  where
    gen tenpow p =
        let ps =
                [ nextP
                | x <- [1 .. 9] -- no 10 because then truncating would skip a digit and that doesn't count >:(
                , let nextP = p + x * tenpow
                , nextP <= limit && isPrime nextP
                ]
        in ps ++ concatMap (gen (tenpow * 10)) ps

solve :: Integral a => a
solve =
    let rl = sort rightLeftTruncPrimes
        lim = last rl
        lr = sort $ leftRightTruncPrimes lim
    in sum' $ rl `intersect` lr

p037 :: IO ()
p037 = print solve
