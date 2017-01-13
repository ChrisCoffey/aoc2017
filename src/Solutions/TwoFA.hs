module Solutions.TwoFA (
    partA,
    partB
) where

import Data.Matrix hiding (toList, trace, (<|>))
import qualified Data.Matrix as MX
import Data.Vector (toList)
import Text.Parsec.String (Parser)
import Text.Parsec

import Debug.Trace

partA :: IO ()
partA = do
    ls <- readFile "data/2FA.txt"
    case runParser doParse () "" ls of
        Left e -> print $ show e
        Right is -> do
            print freshScreen
            let (Screen m) = foldl interpret freshScreen is
            let res = length . filter (== '#') $ MX.toList m
            print $ show res

partB :: IO ()
partB = do
    ls <- readFile "data/2FA.txt"
    case runParser doParse () "" ls of
        Left e -> print $ show e
        Right is -> do
            let (Screen m) = foldl interpret freshScreen is
            print m

newtype Screen = Screen (Matrix Char)
    deriving (Show)

data Instruction
    = Rect {width:: Int, height:: Int}
    | RotateRow {row:: Int, places:: Int}
    | RotateCol {col :: Int, places :: Int}
    deriving (Show)

interpret :: Screen -> Instruction -> Screen
interpret (Screen m) (Rect w h) = 
    Screen $ setSubMatrix m '#' w h
interpret (Screen m) (RotateRow row places) = 
    Screen $ rotateRow m row places
interpret (Screen m) (RotateCol col places) = 
    Screen $ rotateColumn m col places

freshScreen :: Screen
freshScreen = Screen $ matrix 6 50 (const ' ')

rotateRow :: Matrix a-> Int -> Int -> Matrix a
rotateRow m r places = 
    foldl rotateElem m row
    where
    row = zip [1..] . toList $ getRow r m
    l = ncols m
    rotateElem mx (i,e) = let
        ix = 1 + ((i + (places -1)) `mod` l)
        in setElem e (r, ix) mx

rotateColumn :: Matrix a -> Int -> Int -> Matrix a
rotateColumn m c places = 
    foldl rotateElem m col
    where
    col = zip [1..] . toList $ getCol c m
    l = nrows m
    rotateElem mx (i,e) = let
        ix = 1 + ((i + (places - 1)) `mod` l)
        in setElem e (ix, c) mx

setSubMatrix :: Matrix a -> a -> Int -> Int -> Matrix a
setSubMatrix m v w h = 
    foldl f m [(x,y)| x <- [1..h], y <- [1..w]]
    where
    f mx (x,y) = setElem v (x,y) mx

doParse :: Parser [Instruction]
doParse = do
    instructions <- (parseRect <|> parseRowRotation <|> parseColumnRotation) `sepEndBy` newline
    eof
    pure instructions

parseRect :: Parser Instruction
parseRect = do
    _ <- try (string "rect")
    space
    r <- many1 digit
    let w = read r
    char 'x'
    c <- many1 digit
    let h = read c
    pure Rect {width = w, height = h}

parseRowRotation :: Parser Instruction
parseRowRotation = do
    _ <- try (string "rotate row y=")
    r <- many1 digit
    let row = read r
    _ <- string " by "
    p <- many1 digit
    let places = read p
    pure RotateRow {row = row + 1, places = places}

parseColumnRotation :: Parser Instruction
parseColumnRotation = do
    _ <- try (string "rotate column x=")
    c <- many1 digit
    let col = read c
    _ <- string " by "
    p <- many1 digit
    let places = read p
    pure RotateCol {col = col + 1, places = places }

