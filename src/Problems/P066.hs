module Problems.P066
    ( p066
    ) where

{-

x^2 - Dy^2 = 1

x^2 - Dy^2 = (x + ay)(x - by) = x^2 - bxy + axy + aby^2
where
    ab = D
Because D is not square, a /= b. But, for the abs(axy) == abs(bxy), a must == b
With this contradiction, we know that x^2 - Dy^2 = 1 cannot be represented by (x + ay)(x - by) = 1

Additionally, because D is not square, D > 1 (as 1 is a square)

We also must have x^2 > Dy^2, and because D >= 2,
    x^2 > y^2 -> x > y
              -> x > y*sqrt(2)
              -> x > y*sqrt(D)
              -> x / y > sqrt(D)
To be generous, we will approximate this to:
    x / y > floor(sqrt(D))

x^2 - Dy^2 = 1
x^2 - 1 = Dy^2
(x^2 - 1) / y^2 = D

x^2 = Dy^2 + 1

So we have:
    x = sqrt(Dy^2 + 1)
    x / y > sqrt(D)

So, our bounds are:
    D >= 2, not isSquare(D)
    y >= 1
    y * sqrt(D) < x <= sqrt(Dy^2 + 1)

For generousity, we can say:
    floor(y * sqrt(D)) < x <= ceil(sqrt(Dy^2 + 1))
    y * intSqrt(D) < x <= intSqrt(Dy^2 + 1) + 1

SCRATCH THAT I MISSED OVER SOMETHING VERY IMPORTANT
x = sqrt(Dy^2 + 1), right?
x EQUALS ...
not "x less than or equal", but x EQUALS
So we aren't even bounded on x, we just have a solution for x.
What we actually need to find is the lower bound of y,
    because too low of a y will generate a bad x.

...

Ok, day 2, omg on the Wikipedia page for Diophantine equations there's example of Diophantine equations
called "Pell" equations, which, uhhh, take the form of x^2 - Dy^2 = 1.

Oops.

...

So I finished the problem, but I realize that there are earlier problems that would probably
help me solve this one better. So I'm gonna come back to this one eventually.

 -}
import Data.BigDecimal  ( BigDecimal, RoundingMode (HALF_EVEN), divide,
                          getScale, getValue )
import Data.BigFloating ( sqr )
import Data.Fixed       ( divMod' )
import Data.List        ( inits, maximumBy )
import Data.Ord         ( comparing )
import Data.Ratio       ( denominator, numerator )
import EulerMath        ( minus )

getContFracTerms :: Integral a => BigDecimal -> [a]
getContFracTerms n =
    let (intPart, fracPart) = n `divMod'` 1
        rFP = divide (1, fracPart) (HALF_EVEN, Just 100) -- arbitrarily large precision
    in if fracPart == 0
            then [intPart]
            else intPart : getContFracTerms rFP

contFrac :: (Foldable t, Fractional a) => t a -> a
contFrac = foldr1 (\a b -> a + 1 / b)

convergents :: Fractional a => BigDecimal -> [a]
convergents n =
    let terms = map fromIntegral $ getContFracTerms n
        termsInits = tail $ inits terms
    in map contFrac termsInits

findXYWithConvergents d =
    let cons = convergents (sqr d (HALF_EVEN, Just 100)) -- arbitrarily large precision
        solutions =
            [ (num, dem)
            | frac <- cons
            , let num = numerator frac
            , let dem = denominator frac
            , num * num - getValue d * dem * dem == 1
            ]
    in head solutions
  where
    dAsInteger = getValue d `div` 10 ^ getScale d

p066 :: IO ()
p066 =
    print $
    maximumBy (comparing (fst . findXYWithConvergents)) $
    takeWhile (<= 1000) notSquares
  where
    notSquares = map fromIntegral $ [2 ..] `minus` map (^ 2) [1 ..]
