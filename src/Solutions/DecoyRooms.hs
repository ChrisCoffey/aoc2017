{-# LANGUAGE RecordWildCards #-}

module Solutions.DecoyRooms (
    realRoomsA,
    realRoomsB
) where

import qualified Data.Map as M


import Data.List (groupBy, sort, sortBy, intercalate)
import Data.Ord (comparing)
import Data.Char (chr, ord)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, digit, newline)
import Text.Parsec (manyTill, parse, char, sepEndBy, eof,
                    many1, space, try, anyChar, count,
                    lookAhead, letter)

import Debug.Trace

realRoomsA :: IO ()
realRoomsA = do
    raw <- readFile "data/DecoyRooms.txt"
    case parse doParse "" raw of
        Left e -> error $ show e
        Right rooms -> do 
            let res = sum . fmap section $ filter isRealRoom rooms
            print $ show res

realRoomsB :: IO ()
realRoomsB = do
    raw <- readFile "data/DecoyRooms.txt"
    case parse doParse "" raw of
        Left e -> error $ show e
        Right rooms -> do 
            print "boo"
            let res =  northPoleRoom <$> rooms
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

northPoleRoom :: Room -> Room 
northPoleRoom Room {..} = let
    nw = fmap shiftCypher <$> nameWords
    in Room {name = intercalate " " nw, section = section, checkSum = checkSum}
    where
    nameWords = case parse parseWords "" name of
        Left e -> []
        Right ws -> ws
    shiftCypher c = chr . (+) 97 $ ((ord c - 97) + (section `mod` 26)) `mod` 26 
     

parseWords :: Parser [String]
parseWords = many1 (letter) `sepEndBy` (char '-')
    

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
    
