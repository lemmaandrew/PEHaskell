module EulerUtil
    ( problemInputPath
    , getProblemInput
    , sum'
    , product'
    , minus
    , union
    , joinT
    , gapsW
    , primeWheel
    , Matrix(..)
    , identity
    , matTranspose
    , (|^|)
    , Nat
    , lazyLen
    ) where

import           Control.Applicative ( liftA2 )
import           Data.List           ( foldl', transpose )
import           Data.Map            ( Map, (!?) )
import qualified Data.Map.Strict     as M
import           Text.Printf         ( printf )

problemInputPath :: FilePath
problemInputPath = "problem_inputs/"

-- problemInputPath = "/home/peter/programs/ProjectEuler/PEHaskell/problem_inputs/"
getProblemInput :: Int -> IO String
getProblemInput n =
    let fp = printf "%sP%03d.txt" problemInputPath n
    in readFile fp

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldl' (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldl' (*) 1

-- Ordered lists, difference and union
-- | Non-decreasing list difference
minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) =
    case compare x y of
        LT -> x : minus xs (y : ys)
        EQ -> minus xs ys
        GT -> minus (x : xs) ys
minus xs _ = xs

-- | Non-decreasing list union
union :: Ord a => [a] -> [a] -> [a]
union (x:xs) (y:ys) =
    case compare x y of
        LT -> x : union xs (y : ys)
        EQ -> x : union xs ys
        GT -> y : union (x : xs) ys
union xs [] = xs
union [] ys = ys

-- Tree and wheel operations
-- | Join tree
joinT :: Ord a => [[a]] -> [a]
joinT ((x:xs):t) = x : union xs (joinT (pairs t))
  where
    pairs (xs':ys:t') = union xs' ys : pairs t'

gapsW :: (Ord a, Num a) => a -> [a] -> [a] -> [a]
gapsW k (d:w) s@(c:cs)
    | k < c = k : gapsW (k + d) w s
    | otherwise = gapsW (k + d) w cs

hitsW :: (Ord a, Num a) => a -> [a] -> [a] -> [[a]]
hitsW k (d:w) s@(p:ps)
    | k < p = hitsW (k + d) w s
    | otherwise = scanl (\c d -> c + p * d) (p * p) (d : w) : hitsW (k + d) w ps

primeWheel :: Num a => [a]
primeWheel =
    2 :
    4 :
    2 :
    4 :
    6 :
    2 :
    6 :
    4 :
    2 :
    4 :
    6 :
    6 :
    2 :
    6 :
    4 :
    2 :
    6 :
    4 :
    6 :
    8 :
    4 :
    2 :
    4 :
    2 :
    4 :
    8 :
    6 :
    4 :
    6 :
    2 :
    4 :
    6 :
    2 : 6 : 6 : 4 : 2 : 4 : 6 : 2 : 6 : 4 : 2 : 4 : 2 : 10 : 2 : 10 : primeWheel

data Matrix a =
    Matrix
        { deMatrix :: [[a]]
        , numRows  :: Int
        , numCols  :: Int
        }
    deriving (Eq, Show)

instance Functor Matrix where
    fmap f (Matrix xss n m) = Matrix (fmap (fmap f) xss) n m

instance Applicative Matrix where
    pure a = Matrix [[a]] 1 1
    (Matrix fss a b) <*> (Matrix xss n m)
        | a == n && n == m = Matrix (zipWith (zipWith id) fss xss) n m

instance Num a => Num (Matrix a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (Matrix xss n _) * (Matrix yss _ m) =
        Matrix [[sum' $ zipWith (*) xs ys | ys <- transYss] | xs <- xss] n m
      where
        transYss = transpose yss
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

identity :: Num a => Int -> Int -> Matrix a
identity n k = Matrix ident n n
  where
    ident =
        let ident' =
                case signum k of
                    (-1) -> replicate (abs k) (repeat 0) ++ infiniteIdent
                    0    -> infiniteIdent
                    1    -> drop k infiniteIdent
        in shapeII ident'
    shapeII = map (take n) . take n
    infiniteIdent = (1 : repeat 0) : fmap (0 :) infiniteIdent

matTranspose :: Matrix a -> Matrix a
matTranspose (Matrix xss n m) = Matrix (transpose xss) m n

infixr 8 |^|

(|^|) :: (Num a, Integral b) => Matrix a -> b -> Matrix a
(|^|) m = go (identity (numRows m) 0) (matTranspose m)
  where
    go res _ 0 = res
    go res tm n =
        let res' =
                if odd n
                    then res *| tm
                    else res
        in go res' (tm * tm) (n `div` 2)
    (Matrix xss n _) *| (Matrix yss _ m) =
        Matrix [[sum' $ zipWith (*) xs ys | ys <- yss] | xs <- xss] n m

data Nat
    = Z
    | S Nat
    deriving (Eq, Ord, Show)

instance Enum Nat where
    fromEnum = go 0
      where
        go n Z     = n
        go n (S x) = go (n + 1) x
    toEnum = foldr (const S) Z . enumFromTo 1

instance Num Nat where
    Z + y = y
    x + Z = x
    x + S y = S x + y
    Z * _ = Z
    _ * Z = Z
    x * S y = x + x * y
    abs = id
    fromInteger = foldr (const S) Z . enumFromTo 1
    signum Z = 0
    signum _ = 1
    x - Z = x
    Z - _ = Z
    S x - S y = x - y

lazyLen :: Foldable t => t a -> Nat
lazyLen = foldr (const S) Z
