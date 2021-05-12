module Problems.P028
    ( p028
    ) where

{-
73 74 75 76 77 78 79 80_81
72_43 44 45 46 47 48_49 50
71 42_21 22 23 24_25 26 51
70 41 20 _7  8 _9 10 27 52
69 40 19  6 _1  2 11 28 53
68 39 18 _5  4 _3 12 29 54
67 38_17 16 15 14_13 30 55
66_37 36 35 34 33 32_31 56
65 64 63 62 61 60 59 58_57

1 -> 3 -> 13 -> 31 -> 57
1 -> 5 -> 17 -> 37 -> 65
1 -> 7 -> 21 -> 43 -> 73
1 -> 9 -> 25 -> 49 -> 81

1  -> 3  -> 5  -> 7  -> 9  (+2, 4 times)
9  -> 13 -> 17 -> 21 -> 25 (+4, 4 times)
25 -> 31 -> 37 -> 43 -> 49 (+6, 4 times)

Examining bottom-right diagonal:
1 -> 3 -> 13 -> 31 -> 57
  2 -> 10 -> 18 -> 26
      8 -> 8 -> 8

Examining bottom-left diagonal:
1 -> 5 -> 17 -> 37 -> 65
  4 -> 12 -> 20 -> 28
      8 -> 8 -> 8

Examining top-left diagonal:
1 -> 7 -> 21 -> 43 -> 73
  6 -> 14 -> 22 -> 30
      8 -> 8 -> 8

Examining top-right diagonal:
1 -> 9 -> 25 -> 49 -> 81
  8 -> 16 -> 24 -> 32
      8 -> 8 -> 8

f'' x = 8
f' x = 8*x + c
f x = 4*x^2 + c*x + d
f x = x*(4*x + c) + d

No matter which direction we go:
f 0 = 1 = 0*(4*0 + c) + d
   -> d = 1

f 1 = n = 1*(4*1 + c) + 1
   -> n = 4 + c + 1
   -> n = 5 + c
   -> c = n - 5

Assuming that diagonal goes 1 -> 3 -> 5 -> 7 -> 9 -> 13 -> ...
f 1 = 3
   -> c = 3 - 5 = -2
f x = x*(4*x - 2) + 1
Assuming that diagonal goes 1 -> 5 -> 17 -> 37 -> 65 -> ...
f 1 = 5
   -> c = 5 - 5 = 0
f x = x*(4*x + 0) + 1
Assuming that diagonal goes 1 -> 7 -> 21 -> 43 -> 73 -> ...
f 1 = 7
   -> c = 7 - 5 = 2
f x = x*(4*x + 2) + 1
Assuming that diagonal goes 1 -> 9 -> 25 -> 49 -> 81 -> ...
f 1 = 9
   -> c = 9 - 5 = 4
f x = x*(4*x + 4) + 1

Letting:
    x      = depth into the spiral (starting at 0)
    f x c  = x*(4*x + c) + 1
    sd x c = sum of diagonal = sum (`f` c) [0 .. x]
    sds x  = sum of diagonals = sd x (-2) + sd x 0 + sd x 2 + sd x 4 - 3
    subtract 3 from sds to account for extra ones

sds [0 .. 10] = [1,25,101,261,537,961,1565,2381,3441,4777,6421]

Using numpy.polyfit(np.arange(11), [1,25,101,261,537,961,1565,2381,3441,4777,6421]):
    sds x = (16/3)*x^3 + 10*x^2 + (26/3)*x + 1

 -}
import Data.Ratio ( Ratio, (%) )

solve :: (Integral a, Integral b) => Ratio a -> b
solve x = round $ 16 % 3 * x ^ 3 + 10 * x ^ 2 + 26 % 3 * x + 1

p028 :: IO ()
p028 =
    let depth = (1001 - 1) `div` 2
    in print $ solve (toRational depth)
