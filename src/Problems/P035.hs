module Problems.P035
    ( p035
    ) where

import Data.IntSet ( empty, fromList, member, size, union )
import EulerMath ( isPrime, primesSA )

{-

Really only two rules:
    Letting p be a prime number, if p > 5, then:
        1. p cannot include an even digit
        3. p cannot include 5
    This is because for some rotation of p, n will either be even or divisible by 5

 -}

isCircularPrime :: (Integral a, Show a) => a -> Maybe [a]
isCircularPrime p
    | p > 5 && any (`elem` "024568") (show p) = Nothing
    | otherwise = sequence $ Just p : go (rotate p)
  where
    rotFactor = 10^(length (show p) - 1)
    rotate n =
        let (d, m) = divMod n 10
        in m * rotFactor + d
    go n
        | n == p = []
        | isPrime n = Just n : go (rotate n)
        | otherwise = [Nothing]

solve :: Int -> Int
solve limit = go empty (takeWhile (< limit) primesSA)
  where
    go is [] = size is
    go is (p:ps)
        | p `member` is = go is ps
        | otherwise = case isCircularPrime p of
            Just xs -> go (fromList xs `union` is) ps
            _ -> go is ps

p035 :: IO ()
p035 = print $ solve 1000000
