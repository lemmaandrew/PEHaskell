module Problems.P022
    ( p022
    ) where

import Data.List ( sort )
import Data.List.Split ( wordsBy )
import EulerUtil ( getProblemInput, sum' )

loadData :: IO [String]
loadData = wordsBy (`elem` "\",") <$> getProblemInput 22

solve :: [String] -> Int
solve probData = sum' $ zipWith (*) scores [1..]
  where
    scores = map getScore $ sort probData
    getScore word = sum' [fromEnum x - 64 | x <- word]

p022 :: IO ()
p022 = print . solve =<< loadData
