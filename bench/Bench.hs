import Criterion.Main
import EulerMath

testnums :: Num a => [a]
testnums =
    [ 104395301 -- large prime
    , 367567200 -- superior highly composite number
    , 102334155 -- fibonacci number
    ]

testSuite :: [Benchmark]
testSuite =
    [ bgroup "isPrime" $ buildBench isPrime
    , bgroup "primeFactors" $ buildBench primeFactors
    , bgroup "factors" $ buildBench factors
    , bgroup "numFactors" $ buildBench numFactors
    , bgroup "sumFactors" $ buildBench sumFactors
    , bgroup "fib" $ buildBench fib
    ]
  where
    buildBench f = [bench (show n) $ whnf f n | n <- testnums]

main :: IO ()
main = defaultMain testSuite
