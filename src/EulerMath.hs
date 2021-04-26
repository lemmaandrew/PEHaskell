-- Math module
module EulerMath
    ( minus
    , sum'
    , sumRange
    , isPrime
    , primeFactors
    , factors
    , primes
    , xnacci
    , xnaccis
    , fib
    , fibs
    , Matrix(..)
    , identity
    , Nat
    , lazyLen
    ) where

import Data.List
import EulerUtil

-- | Sum of range [a, a + d .. d]
sumRange a b d =
    let n = (b - a) `div` d + 1
        b' = (n - 1) * d + a -- n - 1 to exclude first term (a)
    in (n * (a + b')) `div` 2

-- | Simple determininistic primality test
isPrime :: Integral a => a -> Bool
isPrime n
    | n < 4 = n > 1
    | even n || n `mod` 3 == 0 = False
    | otherwise =
        let candidates = takeWhile (\x -> x * x <= n) [5,11 ..]
        in all (\x -> n `mod` x /= 0 && n `mod` (x + 2) /= 0) candidates

-- | List out the prime factors of a number, repeating factors as needed
--
-- e.g.,
--
-- primeFactors 54 == [2, 3, 3, 3]
primeFactors :: Integral a => a -> [a]
primeFactors = go primes
  where
    go (p:ps) n
        | p >= n = [p | p == n]
        | otherwise =
            let (d, m) = divMod n p
            in if m == 0
                    then p : go (p : ps) d
                    else go ps n

factors :: Integral a => a -> [(a, a)]
factors n = [(i, d) | i <- candidates, let (d, m) = divMod n i, m == 0]
  where
    candidates = takeWhile (\x -> x * x <= n) [1 ..]

-- | Infinite primes with wheel
--
-- From: https://wiki.haskell.org/Prime_numbers#Tree_merging_with_Wheel
primes :: Integral a => [a]
primes =
    [2, 3, 5, 7] ++
    _Y ((11 :) .
        tail .
        gapsW 11 primeWheel .
        joinT .
        map
            (\p ->
                 map (p *) . dropWhile (< p) $
                 scanl (+) (p - rem (p - 11) 210) primeWheel))
  where
    _Y g = g (_Y g) -- Fixpoint combinator that prevents memory leaks from unnecessary memoization

-- | Get the nth element of a Fibonacci-esque sequence
xnacci :: (Integral b, Num a) => [a] -> b -> a
xnacci initial n = head $ head $ deMatrix $ matInit * basis ^ n
  where
    matInit = Matrix [initial]
    basis = setLastToOne $ identity (length initial) (-1)
    setLastToOne (Matrix xss) = Matrix [init xs ++ [1] | xs <- xss]

-- | Generate Fibonacci-esque sequence from initial sequence
xnaccis :: Num a => [a] -> [a]
xnaccis (x:xs) = xnaccis'
  where
    xnaccis' = xs <> scanl' (+) x xnaccis'

fibs :: Num a => [a]
fibs = xnaccis [1, 1]

fib :: (Integral a, Num b) => a -> b
fib = xnacci [1, 1]
