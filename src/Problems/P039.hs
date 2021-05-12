module Problems.P039
    ( p039
    ) where

{-

From Problem 9:
    For a Pythagorean triple (a, b, c) where a^2 + b^2 = c^2,
    and some integers m, n, k:
        k > 0,
        m > n > 0,
        coprime m, n

        a = k * (m^2 - n^2)
        b = k * (2*m*n)
        c = k * (m^2 + n^2)

    a + b + c <= 1000
    k * (m^2 - n^2) + k * (2*m*n) + k * (m^2 + n^2) <= p

    n = (500 - k*m^2) / k*m, 2*k*m /= 0
    So, with k = 1:
      n = (500 - m^2) / m
      So, m < floor (sqrt 500)
      So, m < 22

    m decreases as k increases,
    so we will only need to worry about 0 < n < m < 22

Now notes for this problem :D

Trying to find an upper bound for k:
    n = 1, m = 2
    k * (m^2 - n^2) + k * (2*m*n) + k * (m^2 + n^2) <= 1000
    k * (2^2 - 1^2) + k * (2*2*1) + k * (2^2 + 1^2) <= 1000
    k * 3 + k * 4 + k * 5 <= 1000
    12 * k <= 1000
    k <= 83

 -}
import           Data.Bifunctor
import           Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import           Data.List
import           Data.Ord
import           Data.Set           ( Set )
import qualified Data.Set           as S

genTriple :: Num a => a -> a -> a -> (a, a, a)
genTriple k m n =
    let m' = m * m
        n' = n * n
        a = m' - n'
        b = 2 * m * n
        c = m' + n'
    in (k * a, k * b, k * c)

sortTriple :: Ord a => (a, a, a) -> (a, a, a)
sortTriple (a, b, c) = (\[x, y, z] -> (x, y, z)) $ sort [a, b, c]

solve :: Int
solve = fst $ maximumBy (comparing snd) $ map (second S.size) $ IM.toList im
  where
    im :: IntMap (Set (Int, Int, Int))
    im =
        foldr
            (uncurry (IM.insertWith S.union))
            IM.empty
            [ (p, S.singleton t)
            | n <- [2 .. 20]
            , m <- [n + 1 .. 21]
            , k <- [1 .. 83]
            , let t@(a, b, c) = sortTriple $ genTriple k m n
            , let p = a + b + c
            , p <= 1000
            ]

p039 :: IO ()
p039 = print solve
