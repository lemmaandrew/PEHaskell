module Problems.P019
    ( p019
    ) where

import Data.List.Split
    ( dropInitBlank, keepDelimsL, split, splitPlaces, whenElt )
import EulerMath ( sum' )

data Day
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Enum, Ord, Show)

type Year = Int

isLeapYear :: Year -> Bool
isLeapYear year =
    year `mod` 4 == 0 && year `mod` 100 /= 0 || year `mod` 400 == 0

monthLengths :: Year -> [Int]
monthLengths year =
    [ 31                               -- January
    , 28 + fromEnum (isLeapYear year)  -- February
    , 31                               -- March
    , 30                               -- April
    , 31                               -- May
    , 30                               -- June
    , 31                               -- July
    , 31                               -- August
    , 30                               -- September
    , 31                               -- October
    , 30                               -- November
    , 31                               -- December
    ]

-- | Number of leap years between Jan 1 of the start year and Jan 1 of the end year.
--
-- Meaning, the last year is not includedin the count, but the first year is
leapYearsBetween :: Year -> Year -> Int
leapYearsBetween start end = leapYearsBefore end - leapYearsBefore start
  where
    leapYearsBefore year =
        let year' = year - 1
        in (year' `div` 4) - (year' `div` 100) + (year' `div` 400)

-- | Gets the day of January 1 of a year, given year >= 1900
firstDayOfYear :: Year -> Day
firstDayOfYear year =
    toEnum $ (year - 1900 + leapYearsBetween 1900 year) `mod` 7

buildCalendar :: Year -> [[[Day]]]
buildCalendar year = calendar
  where
    monthLens = monthLengths year
    days = dropWhile (/= firstDayOfYear year) $ cycle [Monday .. Sunday]
    months = take 12 $ splitPlaces monthLens days
    splitter = dropInitBlank $ keepDelimsL (whenElt (== Monday))
    calendar = map (split splitter) months

solve :: Int
solve = sum' $ map solve' [1901 .. 2000]
  where
    solve' year = length [() | ((Sunday : _) : _) <- buildCalendar year]

p019 :: IO ()
p019 = print solve
