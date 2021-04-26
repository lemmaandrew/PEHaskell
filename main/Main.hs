module Main where

import Control.Monad      ( forM_ )
import Problems           ( problems )
import System.Environment
import Text.Printf        ( printf )

usage :: String
usage = "Usage: stack run PEHaskell num\n  where num = problem number"

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn usage
        else forM_ args $ \n ->
                 let (p, f) = problems !! (read n - 1)
                 in printf "Running problem %03d\n\n" p >> f
