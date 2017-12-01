{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module  Solutions.RadioIsotope (
    partA
) where

import qualified Data.Set as S

import Control.Monad (when, guard)
import Data.Maybe (maybe)
import Data.List ((\\))
import Text.Parsec
import Text.Parsec.String

newtype Floor = Floor {contents:: S.Set Part}
    deriving (Eq, Show)

data Part
    = RTG {rElem :: Char}
    | Chip {cElem :: Char}
    deriving (Eq, Show, Ord)

data CS = CS {
    csLevel :: Int,
    csFloors :: [Floor],
    csSeenConfigs :: S.Set (S.Set Floor)
    } deriving (Eq, Show)

partA :: IO ()
partA = do
    fs <- floors
    print fs

floors :: IO CS
floors = do
    ls <- readFile "data/RadioIsotope.txt"
    case runParser doParse () "" ls of
        Left e -> error $ show e
        Right floors -> pure CS {csLevel = 0, csFloors = floors}

possibleMoves :: CS -> [CS]
possibleMoves cs@(CS {..}) = do
    lvl <- [l | l <- [csLevel -1, csLevel +1], l `elem` [0,1,2,3]]
    val <- singles ++ doubles
    maybe mempty pure (tryMove cs lvl val)
    where
    singles = (flip Single (floorNum + 1) <$> contents f) ++ (flip Single (floorNum - 1) <$> contents f)
    doubles = let
        ps = pairs $ contents f
        up = (\(a,b)-> Double a b (floorNum + 1)) <$> ps
        down = (\(a,b)-> Double a b (floorNum - 1)) <$> ps
        in up  ++ down

tryMove :: CS -> Int -> S.Set Part -> Maybe CS
tryMove (CS {..}) targetFloor move = do
    let fs = contents $ csFloors !! targetFloor
        fs' = move `S.union` fs
    guard $ validFloor fs'
    let cs' = undefined
    Nothing

validFloor :: S.Set Part -> Bool
validFloor parts = let
    gs = getElem <$> generators parts
    cs = getElem <$> chips parts
    in all (`elem` gs) cs

pairs :: [a] -> [(a,a)]
pairs [a,b] = [(a,b)]
pairs (x:xs) = ((x,) <$> xs) ++ pairs xs

{-
 -}
unfold ::
    (seed -> trackedState -> ([seed], trackedState))  ->
    trackedState ->
    [seed] ->
    [seed]
unfold _ _ [] = []
unfold f state zs@(x:xs) = zs ++ unfold f nextState steps
    where
    g s [] = ([], s)
    g s (a:as) = let
        (as', s') = f a s
        (as'', s'') = g s' as
        in (as' ++ as'', s'')
    (steps, nextState) = g state xs



-- if no generators, can always move a chip
-- if generators, all chips must have generator
-- if no chips, can always move a generator
-- if no chips, can always move a generator + chip

generators :: Floor -> String
generators = fmap getElem . filter isGenerator . contents
    where
    isGenerator (RTG _) = True
    isGenerator _ = False

chips :: Floor -> String
chips = fmap getElem . filter isChip . contents
    where
    isChip (Chip _) = True
    isChip _ = False

getElem :: Part -> Char
getElem (RTG e) = e
getElem (Chip e) = e

--
-- Parser
--

doParse :: Parser [Floor]
doParse = sepEndBy parseFloor newline

parseFloor :: Parser Floor
parseFloor = do
    parts <- sepBy parsePart (char ' ') <|> parseEmptyFloor
    pure $ Floor parts

parseEmptyFloor :: Parser [Part]
parseEmptyFloor = do
    string "nothing"
    pure []

parsePart :: Parser Part
parsePart = parseChip <|> parseRTG

parseChip :: Parser Part
parseChip =
    try (do
        char 'M'
        x <- anyChar
        pure Chip {cElem = x}
    )

parseRTG :: Parser Part
parseRTG = try (do
    char 'G'
    x <- anyChar
    pure RTG {rElem = x}
    )
