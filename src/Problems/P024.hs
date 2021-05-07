module Problems.P024
    ( p024
    ) where

import Data.Char ( intToDigit )
import Data.List ( scanl' )

facts :: (Enum a, Num a) => [a]
facts = scanl' (*) 1 [1 ..]

solve :: Int -> [Int]
solve = go (reverse $ take 10 facts) [0 .. 9] . subtract 1
  where
    go [] [] 0 = []
    go (f:fs) digits n =
        let (digiIdx, rem) = quotRem n f
            (digit, digits') = removeDigit digiIdx digits
        in digit : go fs digits' rem
    removeDigit d xs =
        let (before, a:after) = splitAt d xs
        in (a, before ++ after)

p024 :: IO ()
p024 =
    let digits = solve 1000000
        digiStr = concatMap (return . intToDigit) digits
    in putStrLn digiStr
