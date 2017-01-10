module Solutions.SignalsNoise (
    signalA,
    signalB
) where

import Data.Ord (comparing)
import Data.List (maximumBy, minimumBy, sort, group)

signalA :: IO ()
signalA = do
    ls <- lines <$> readFile "data/Signals.txt"
    let res = fmap head . grouped . transpose $ ls
    print res

signalB :: IO ()
signalB = do
    ls <- lines <$> readFile "data/Signals.txt"
    let res = fmap head . grouped' . transpose $ ls
    print res

transpose :: [[a]] -> [[a]]
transpose xs = (head <$> xs):transpose (tail <$> xs)

grouped :: Ord a => [[a]] -> [[a]]
grouped xs = maximumBy (comparing length) . group . sort <$> xs

grouped' :: Ord a => [[a]] -> [[a]]
grouped' xs = minimumBy (comparing length) . group . sort <$> xs
