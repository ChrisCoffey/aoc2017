{-# LANGUAGE OverloadedStrings #-}

module Solutions17.Day2 where

import Data.Char (isDigit, digitToInt)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T

rowCheckSum ::
    T.Text
    -> Int
rowCheckSum raw = let
    fields = read . T.unpack <$> T.splitOn "\t" raw
    max = maximum fields
    min = minimum fields
    in max - min

rowCheckSum2 ::
    T.Text
    -> Int
rowCheckSum2 raw = let
    fields = sort $ (read . T.unpack) <$> T.splitOn "\t" raw
    in go fields
    where
        evenlyDivides l r = l `mod` r == 0
        go :: [Int] -> Int
        go [] = 0
        go (x:rest) =
            case filter (`evenlyDivides` x) rest of
                [] -> go rest
                [y] -> y `div` x

checkA :: IO Int
checkA = do
    rows <- T.lines <$> T.readFile "data/2017/Day2.a"
    pure . sum  $ rowCheckSum <$> rows

checkB :: IO Int
checkB = do
    rows <- T.lines <$> T.readFile "data/2017/Day2.a"
    pure . sum  $ rowCheckSum2 <$> rows

