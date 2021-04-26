module Problems.P009
    ( p009
    ) where

{- For a Pythagorean triple (a, b, c) where a^2 + b^2 = c^2,
 - and some integers m, n, k:
 -   k > 0,
 -   m > n > 0,
 -   coprime m, n
 -
 - a = k * (m^2 - n^2)
 - b = k * (2*m*n)
 - c = k * (m^2 + n^2)
 -
 - a + b + c = 1000
 - k * (m^2 - n^2) + k * (2*m*n) + k * (m^2 + n^2) = 1000
 -
 - n = (500 - k*m^2) / k*m, 2*k*m /= 0
 - So, with k = 1:
 -   n = (500 - m^2) / m
 -   So, m < floor (sqrt 500)
 -   So, m < 22
 -
 - m decreases as k increases,
 - so we will only need to worry about 0 < n < m < 22
 -
 - -}
genTriple :: Num a => a -> a -> (a, a, a)
genTriple m n =
    let m' = m * m
        n' = n * n
        a = m' + n'
        b = 2 * m * n
        c = m' - n'
    in (a, b, c)

solve :: (Enum a, Eq a, Num a) => (a, a, a)
solve =
    head $
    [ (a, b, c)
    | n <- [1 .. 20]
    , m <- [n .. 21]
    , let g@(a, b, c) = genTriple m n
    , a + b + c == 1000
    ]

p009 :: IO ()
p009 =
    let (a, b, c) = solve
    in print $ a * b * c
