module Problems.P054
    ( p054
    ) where

import Control.Monad                ( guard )
import Data.Bifunctor               ( Bifunctor (bimap) )
import Data.Char                    ( digitToInt )
import Data.Function                ( on )
import Data.List                    ( groupBy, sortOn )
import Data.Maybe ( catMaybes )
import Data.Ord ( Down(Down), comparing )                     
import EulerMath ( combinations, getProblemInput, lazyLen )
import Text.ParserCombinators.ReadP ( get, pfail, readP_to_S )

data Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades
    deriving (Enum, Eq, Ord)

instance Show Suit where
    show Hearts   = "H"
    show Diamonds = "D"
    show Clubs    = "C"
    show Spades   = "S"

data Card =
    Card
        { val  :: Int
        , suit :: Suit
        }
    deriving (Eq, Ord)

instance Show Card where
    show (Card val suit)
        | val < 10 = show val ++ show suit
        | otherwise = ("TJQKA" !! (val - 10)) : show suit

instance Read Card where
    readsPrec _ = readP_to_S parse
      where
        parse = Card <$> pVal <*> pSuit
        pVal = do
            valChar <- get
            case valChar of
                'T' -> return 10
                'J' -> return 11
                'Q' -> return 12
                'K' -> return 13
                'A' -> return 14
                _ ->
                    let val = digitToInt valChar
                    in guard (2 <= val && val <= 9) >> return val
        pSuit = do
            suitChar <- get
            case suitChar of
                'H' -> return Hearts
                'D' -> return Diamonds
                'C' -> return Clubs
                'S' -> return Spades
                _   -> pfail

newtype Hand =
    Hand [Card]
    deriving (Eq, Show)

handFromStr :: String -> Hand
handFromStr = Hand . map read . words

instance Ord Hand where
    compare = comparing getHandTypes

data HandType
    = HC Card -- High Card
    | OP [Card] -- One Pair
    | TP [[Card]] -- Two Pair
    | TK [Card] -- Three of a Kind
    | ST [Card] -- STraight
    | FL [Card] -- FLush
    | FH [[Card]] -- Full House
    | FK [Card] -- Four of a Kind
    | SF [Card] -- Straight Flush
    | RF [Card] -- Royal Flush
    deriving (Eq, Ord, Show)

-- | Returns all of the HandTypes of a hand ordered from highest to lowest
getHandTypes :: Hand -> [HandType]
getHandTypes (Hand hand) =
    catMaybes [royalFlush, straightFlush] ++
    fourOfAKind ++
    catMaybes [fullHouse, flush, straight] ++
    threeOfAKinds ++ twoPairs ++ onePairs ++ highCards
  where
    sortedHand = sortOn Down hand
    groupedHand = groupBy ((==) `on` val) sortedHand
    highCards = map HC sortedHand
    onePairs = map OP $ filter ((== 2) . lazyLen) groupedHand
    twoPairs = map (\[OP xs, OP ys] -> TP [xs, ys]) $ combinations 2 onePairs
    threeOfAKinds = map TK $ filter ((== 3) . lazyLen) groupedHand
    straight
        | and $
              zipWith
                  (\(Card v1 _) (Card v2 _) -> v1 - 1 == v2)
                  sortedHand
                  (tail sortedHand) = Just $ ST sortedHand
        | otherwise = Nothing
    flush
        | all ((== suit (head sortedHand)) . suit) sortedHand =
            Just $ FL sortedHand
        | otherwise = Nothing
    fullHouse =
        case groupedHand of
            [[_, _, _], [_, _]] -> Just $ FH groupedHand
            [[_, _], [_, _, _]] -> Just $ FH (reverse groupedHand)
            _                   -> Nothing
    fourOfAKind = map FK $ filter ((== 4) . lazyLen) groupedHand
    straightFlush =
        case (straight, flush) of
            (Just _, Just _) -> Just $ SF sortedHand
            _                -> Nothing
    royalFlush =
        case (straight, map val sortedHand) of
            (Just _, [14, 13, 12, 11, 10]) -> Just $ RF sortedHand
            _                              -> Nothing

processProblem :: IO [(Hand, Hand)]
processProblem = do
    cards <- map (map read . words) . lines <$> getProblemInput 54
    let hands = map (bimap Hand Hand . splitAt 5) cards
    return hands

solve :: IO ()
solve = do
    games <- processProblem
    print $ length $ filter (\(p1, p2) -> max p1 p2 == p1) games


p054 :: IO ()
p054 = solve
