module Solutions17.Day4 where

import Data.List (nub, sort)

noDupes ::  (Eq a) =>
    [a]
    -> Bool
noDupes xs = xs == nub xs

containsAnagram ::
    [String]
    -> Bool
containsAnagram [] = False
containsAnagram (word:rest)
    | any (isAnagramOf word) rest = True
    | otherwise = containsAnagram rest


isAnagramOf :: (Ord a, Eq a) =>
    [a]
    -> [a]
    -> Bool
isAnagramOf a b = sort a == sort b

checkA :: IO Int
checkA = do
    lns <- lines <$> readFile "data/2017/Day4.a"
    let validPPs = filter (noDupes . words) lns
    pure $ length validPPs

checkB :: IO Int
checkB = do
    lns <- lines <$> readFile "data/2017/Day4.a"
    let validPPs = filter (not . containsAnagram . words) lns
    pure $ length validPPs
