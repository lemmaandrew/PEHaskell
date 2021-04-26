module Problems.P004
    ( p004
    ) where

isPal :: Show a => a -> Bool
isPal n =
    let sn = show n
    in sn == reverse sn

-- We use the fact that all even-digit palindromes are divisible by 11
-- Proof here: https://www.answers.com/Q/Why_are_all_even_number_digit_palindromes_divisible_by_11
-- Therefore, at least one of the factors must be divisible by 11
solution :: (Enum a, Ord a, Num a, Show a) => a
solution =
    maximum
        [ prod
        | a <- [121,132 .. 999]
        , b <- [101 .. 999]
        , let prod = a * b
        , isPal prod
        ]

p004 :: IO ()
p004 = print solution
