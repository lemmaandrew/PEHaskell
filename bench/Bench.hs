import Control.Exception ( evaluate )
import Control.DeepSeq
import Criterion.Main
import EulerMath
import System.Environment

testnums :: [Int]
testnums =
    [ 104395301 -- large prime
    , 367567200 -- superior highly composite number
    , 102334155 -- fibonacci number
    ]

-- | For Criterion benchmarking
testSuite :: [Benchmark]
testSuite =
    [ bgroup "isPrime" $ buildBench isPrime
    , bgroup "primeFactors" $ buildBench (primeFactors :: Int -> [(Int, Int)])
--    , bgroup "factors" $ buildBench factors
    , bgroup "numFactors" $ buildBench numFactors
    , bgroup "sumFactors" $ buildBench sumFactors
    , bgroup "fib" $ buildBench (fib :: Int -> Int)
    ]
  where
    buildBench f = [bench (show n) $ nf f n | n <- testnums]

profSuite :: [()]
profSuite =
    [ rnf $ map isPrime testnums
    , rnf (map primeFactors testnums :: [[(Int, Int)]])
    , rnf (map factors testnums :: [[(Int, Int)]])
    , rnf (map numFactors testnums :: [Int])
    , rnf (map sumFactors testnums :: [Int])
    , rnf (map fib testnums :: [Int])
    ]

usage :: String
usage = "Usage: \n\
\  If you want Criterion output: stack bench --ba \"--output=directoryname/filename.html\"\n\
\  If you want profiling: stack bench --profile --ba \"p\"\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ['-':'-':_] -> defaultMain testSuite
        ["p"] -> evaluate $ rnf profSuite
        _ -> putStrLn usage

