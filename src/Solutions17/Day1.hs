module Solutions17.Day1 where

import Data.Char (isDigit, digitToInt)
import Data.Monoid ((<>))

newtype CircularList a = CircularList [a]

adjacentMatches :: (Eq a) =>
    CircularList a
    -> [a]
adjacentMatches (CircularList []) = []
adjacentMatches (CircularList xs) =
    map fst . filter matchesNext $ zip xs (tail xs <> [head xs])
    where
        matchesNext (l,r) = l == r

halfwayMatches :: Eq a =>
    CircularList a
    -> [a]
halfwayMatches (CircularList []) = []
halfwayMatches (CircularList xs) = let
    halfLength = length xs `div` 2
    firstHalf = take halfLength xs
    backHalf = drop halfLength xs
    in map fst . filter matchesNext $ zip firstHalf backHalf
    where
        matchesNext (l,r) = l == r


checkA :: IO Int
checkA = do
    raw <- filter isDigit <$> readFile "data/2017/Day1.a"
    let matches = adjacentMatches . CircularList $ digitToInt <$> raw
    pure $ sum matches

checkB :: IO Int
checkB = do
    raw <- filter isDigit <$> readFile "data/2017/Day1.a"
    let matches = halfwayMatches . CircularList $ digitToInt <$> raw
    pure . (* 2) $ sum matches
