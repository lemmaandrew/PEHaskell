module Problems.P017
    ( p017
    ) where

import           Data.Char   ( isAlpha )
import           Data.IntMap ( IntMap, (!) )
import qualified Data.IntMap as IM

ones :: IntMap String
ones =
    IM.fromAscList
        [ (0, "")
        , (1, "one")
        , (2, "two")
        , (3, "three")
        , (4, "four")
        , (5, "five")
        , (6, "six")
        , (7, "seven")
        , (8, "eight")
        , (9, "nine")
        ]

teens :: IntMap String
teens =
    IM.fromAscList
        [ (10, "ten")
        , (11, "eleven")
        , (12, "twelve")
        , (13, "thirteen")
        , (14, "fourteen")
        , (15, "fifteen")
        , (16, "sixteen")
        , (17, "seventeen")
        , (18, "eighteen")
        , (19, "nineteen")
        ]

tens :: IntMap String
tens =
    IM.fromAscList
        [ (20, "twenty")
        , (30, "thirty")
        , (40, "forty")
        , (50, "fifty")
        , (60, "sixty")
        , (70, "seventy")
        , (80, "eighty")
        , (90, "ninety")
        ]

numToWords :: Int -> String
numToWords 1000 = "one thousand"
numToWords n =
    let (hs, ts) = divMod n 100
        hsStr =
            if hs > 0
                then ones ! hs ++ " hundred"
                else ""
        and' =
            if hs > 0 && ts > 0
                then "and"
                else ""
        tsStr
            | ts >= 20 =
                let (ts', os) = divMod ts 10
                    osStr = if os > 0 then ' ' : numToWords os else ""
                in tens ! (ts' * 10) ++ osStr
            | ts >= 10 = teens ! ts
            | otherwise = ones ! ts
    in unwords $ filter (not . null) [hsStr, and', tsStr]

solve :: Int
solve = length $ filter isAlpha $ concatMap numToWords [1 .. 1000]

p017 :: IO ()
p017 = print solve
