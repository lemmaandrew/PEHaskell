module Problems.P027
    ( p027
    ) where

import Data.List ( maximumBy )
import Data.Ord  ( comparing )
import EulerMath

{- n^2 + an + b = p where p is prime
 - For n = 0:
 -   0^2 + 0n + b = p -> b = p
 -   Therefore, b is prime.
 - For n = 1:
 -   1^2 + 1a + b = 1 + a + b = p
 -   Therefore, for the expression to be odd, a or b must be odd.
 -   We know that b will be odd for p > 2, so a must be odd for p > 2.
 -   a cannot be both even and odd at the same time,
 -   so we have a be odd so we can generate more primes than just 2.
 -
 - So, we have:
 -   * b prime
 -   * a odd
 -
 -}
numPrimes :: Int -> Int -> Int
numPrimes a b = go 0
  where
    go n
        | isPrime (n * n + n * a + b) = go (n + 1)
        | otherwise = n

solve :: Int -> Int -> (Int, Int)
solve limitA limitB = maximumBy (comparing (uncurry numPrimes)) pairs
  where
    as = reverse (map negate [1,3 .. limitA - 1]) ++ [1,3 .. limitA - 1]
    bs =
        let ps = takeWhile (< limitB) primesSA
        in reverse (map negate ps) ++ ps
    pairs = map toTuple $ sequence [as, bs]
    toTuple [a, b] = (a, b)

p027 :: IO ()
p027 =
    let (a, b) = solve 1000 1001
    in do putStrLn $ "a:     " ++ show a
          putStrLn $ "b:     " ++ show b
          putStrLn $ "a * b: " ++ show (a * b)
