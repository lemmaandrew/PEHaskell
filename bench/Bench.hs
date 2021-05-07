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
    , bgroup "isPrimeTMWE" $ buildBench isPrimeTMWE
    , bgroup "isPrimeSA" $ buildBench isPrimeSA
    , bgroup "primeFactorsTMWE" $ buildBench (primeFactorsTMWE :: Int -> [(Int, Int)])
    , bgroup "primeFactorsSA" $ buildBench primeFactorsSA
--    , bgroup "factors" $ buildBench factors
    , bgroup "numFactorsTMWE" $ buildBench numFactorsTMWE
    , bgroup "numFactorsSA" $ buildBench numFactorsSA
    , bgroup "sumFactorsTMWE" $ buildBench sumFactorsTMWE
    , bgroup "sumFactorsSA" $ buildBench sumFactorsSA
--    , bgroup "fib" $ buildBench (fib :: Int -> Int)
    ]
  where
    buildBench f = [bench (show n) $ nf f n | n <- testnums]

profSuite :: [()]
profSuite =
    [ --buildBench isPrime
--      buildBench isPrimeTMWE
      buildBench isPrimeSA
--    , buildBench (primeFactorsTMWE :: Int -> [(Int, Int)])
    , buildBench primeFactorsSA
--    , rnf (map factors testnums :: [[(Int, Int)]])
--    , buildBench numFactorsTMWE
    , buildBench numFactorsSA
--    , buildBench sumFactorsTMWE
    , buildBench sumFactorsSA
--    , rnf (map fib testnums :: [Int])
    ]
  where
    buildBench f = rnf $ map f testnums

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

