-- Math module
module EulerMath
    ( module EulerUtil
    , sumRange
    , digits
    , sumDigits
    , factorial
    , primesTMWE
    , primesSA
    , isPrime
    , isPrimeTMWE
    , isPrimeSA
    , primeFactorsTMWE
    , primeFactorsSA
    , factors
    , numFactorsTMWE
    , numFactorsSA
    , sumFactorsTMWE
    , sumFactorsSA
    , xnacci
    , xnaccis
    , invXnacci
    , fib
    , fibs
    , invFib
    ) where

import Data.Array.Unboxed
import Data.List
import Data.Ord ( comparing )
import EulerUtil

-- | sumRange a b d == Sum of range [a, a + d .. b]
sumRange :: Integral a => a -> a -> a -> a
sumRange a b d =
    let n = (b - a) `div` d + 1
        b' = (n - 1) * d + a -- n - 1 to exclude first term (a)
    in (n * (a + b')) `div` 2

-- | Returns the digits of a number
--
-- For example:  
-- digits 12345 == [1,2,3,4,5]
digits :: Integral a => a -> [a]
digits n
    | n < 10 = [n]
    | otherwise =
        let (d, m) = divMod n 10
        in digits d ++ [m]

sumDigits :: Integral a => a -> a
sumDigits = go 0
  where
    go s n
        | n < 10 = s + n
        | otherwise =
            let (d, m) = divMod n 10
            in go (s + m) d

factorial :: Integral a => a -> a
factorial = go 1
  where
    go start n
        | n <= 16 = product [start .. start + n - 1]
        | otherwise =
            let i = n `div` 2
            in go start i * go (start + i) (n - i)

-- | Infinite primes with wheel
--
-- From: https://wiki.haskell.org/Prime_numbers#Tree_merging_with_Wheel
primesTMWE :: Integral a => [a]
primesTMWE =
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

-- | A list of primes generated using unboxed arrays
--
-- Has the distinct advantage of being much faster than primesTMWE, but the disadvantage of only producing
-- up to Int max bound (2^63 - 1)
primesSA :: [Int]
primesSA = 2 : oddprimes ()
  where
    oddprimes = (3 :) . sieve 3 [] . oddprimes
    sieve x fs (p:ps) =
        [i + i + x | (i, True) <- assocs a] ++
        sieve (p * p) ((p, 0) : [(s, rem (y - q) s) | (s, y) <- fs]) ps
      where
        q = (p * p - x) `div` 2
        a :: UArray Int Bool
        a =
            accumArray
                (\_ _ -> False)
                True
                (1, q - 1)
                [(i, ()) | (s, y) <- fs, i <- [y + s,y + s + s .. q]]

-- | Simple determininistic primality test
isPrime :: Integral a => a -> Bool
isPrime n
    | n < 4 = n > 1
    | even n || n `mod` 3 == 0 = False
    | otherwise =
        let candidates = takeWhile (\x -> x * x <= n) [5,11 ..]
        in all (\x -> n `mod` x /= 0 && n `mod` (x + 2) /= 0) candidates

isPrimeTMWE :: Integral a => a -> Bool
isPrimeTMWE n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p*p <= n) primesTMWE

isPrimeSA :: Integral a => a -> Bool
isPrimeSA n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p*p <= n) $ map fromIntegral primesSA

-- | List out the prime factors of a number, repeating factors as needed
--
-- e.g.,
--
-- primeFactorsTMWE 54 == [(2, 1), (3, 3)]
primeFactorsTMWE :: (Integral a, Integral b) => a -> [(a, b)]
primeFactorsTMWE =
    map (\xs@(x:_) -> (x, fromIntegral $ length xs)) . group . go primesTMWE
  where
    go (p:ps) n
        | p >= n = [p | p == n]
        | otherwise =
            let (d, m) = divMod n p
            in if m == 0
                    then p : go (p : ps) d
                    else go ps n

primeFactorsSA :: Int -> [(Int, Int)]
primeFactorsSA =
    map (\xs@(x:_) -> (x, length xs)) . group . go primesSA
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

-- | Number of factors of n, including 1 and itself
numFactorsTMWE :: Integral a => a -> a
numFactorsTMWE = product . map ((+ 1) . snd) . primeFactorsTMWE

numFactorsSA :: Int -> Int
numFactorsSA = product . map ((+ 1) . snd) . primeFactorsSA

-- | Sum of the factors of a number excluding the number itself
--
-- Proof: https://cp-algorithms.com/algebra/divisors.html
sumFactorsTMWE :: Integral a => a -> a
sumFactorsTMWE n =
    let prod =
            product
                [ res
                | (p, a) <- primeFactorsTMWE n
                , let res =
                          if a > 1
                              then (p ^ (a + 1) - 1) `div` (p - 1)
                              else p + 1 -- this resultsin a lot of memory saving (division is expensive)
                ]
    in prod - n

sumFactorsSA :: Int -> Int
sumFactorsSA n =
    let prod =
            product
                [ res
                | (p, a) <- primeFactorsSA n
                , let res =
                          if a > 1
                              then (p ^ (a + 1) - 1) `div` (p - 1)
                              else p + 1 -- this resultsin a lot of memory saving (division is expensive)
                ]
    in prod - n


-- | Get the nth element of a Fibonacci-esque sequence
xnacci :: (Integral b, Num a) => [a] -> b -> a
xnacci initial n = head $ head $ deMatrix $ matInit * basis |^| n
  where
    matInit = Matrix [initial] 1 (length initial)
    basis = setLastToOne $ identity (length initial) (-1)
    setLastToOne (Matrix xss n m) = Matrix [init xs ++ [1] | xs <- xss] n m

-- | Generate Fibonacci-esque sequence from initial sequence
xnaccis :: Num a => [a] -> [a]
xnaccis (x:xs) = xnaccis'
  where
    xnaccis' = xs <> scanl' (+) x xnaccis'

-- | Finds the index of the closest element of a Fibonacci-esque sequence to a number
invXnacci :: (Num a, Ord a, Integral b) => [a] -> a -> b
invXnacci initial n =
    let bigIdx = until (\idx -> xn idx >= n) (*2) 1
    in go (bigIdx `div` 2) bigIdx
  where
    go lidx ridx
        | lidx + 1 >= ridx = minimumBy (comparing (\idx -> abs (xn idx - n))) [lidx, ridx]
        | otherwise =
            let mididx = (lidx + ridx) `div` 2
                mid = xn mididx
            in case compare n mid of
                LT -> go lidx mididx
                EQ -> mididx
                GT -> go mididx ridx
    xn = xnacci initial

fibs :: Num a => [a]
fibs = xnaccis [1, 1]

fib :: (Integral a, Num b) => a -> b
fib = xnacci [1, 1]

invFib :: (Num a, Ord a, Integral b) => a -> b
invFib = invXnacci [1, 1]
