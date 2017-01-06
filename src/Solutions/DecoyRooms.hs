{-# LANGUAGE RecordWildCards #-}

module Solutions.DecoyRooms (
    realRoomsA
) where

import qualified Data.Map as M

import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, digit, newline)
import Text.Parsec (manyTill, parse, char, sepEndBy, eof,
                    many1, space, try, anyChar, count,
                    lookAhead)
import Data.List (groupBy, sort, sortBy)
import Data.Ord (comparing)

import Debug.Trace

realRoomsA :: IO ()
realRoomsA = do
    raw <- readFile "data/DecoyRooms.txt"
    case parse doParse "" raw of
        Left e -> error $ show e
        Right rooms -> do 
            let res = sum . fmap section $ filter isRealRoom rooms
            print $ show res

data Room = Room {
    name :: String,
    section :: Int,
    checkSum :: String
} deriving (Show)

isRealRoom :: Room -> Bool
isRealRoom Room {..} = let
    a = trace checkSum checkSum
    b = trace (take 5 letterScores) (take 5 letterScores)
    in a == b
    where
    letterScores = concatMap (sort . fmap fst) --alphabetize
        . sortBy compareByHead  -- sort groups by frequency
        . groupBy bySecond  -- group by frequency
        . M.toList $ foldr f M.empty name -- count occurances
    f '-' acc = acc
    f c acc = M.insertWith (+) c 1 acc
    bySecond (a,n) (b, m) = n == m
    compareByHead (a:as) (b:bs) = comparing snd b a

doParse :: Parser [Room]
doParse = do
    rooms <- parseRoom `sepEndBy` newline
    eof
    pure rooms

parseRoom :: Parser Room
parseRoom = do
    name <- manyTill anyChar (lookAhead digit)
    n <- many1 digit
    let sector = read n
    char '['
    checksum <- count 5 anyChar
    char ']'
    pure $ Room name sector checksum
    
