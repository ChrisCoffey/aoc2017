{-# LANGUAGE RecordWildCards #-}

module Solutions.StreetGrid (
    streetGridA,
    streetGridB
) where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, digit)
import Text.Parsec (many1, parse, sepBy, skipMany, space, char)

data Direction = N | S | E | W
    deriving (Eq, Show)

data Instruction = L Int | R Int deriving (Show)

data Path = Path {
    north :: Int,
    east :: Int,
    facing :: Direction
} deriving (Eq, Show)

data PathB = PathB {
    northB :: Int,
    eastB :: Int,
    facingB :: Direction,
    visited :: [(Int, Int)]
} deriving Show

streetGridA :: IO ()
streetGridA = do
    raw <- readFile "data/StreetGridA.txt"
    case parse doParse "" raw  of
        Left e -> error $ show e
        Right is -> do 
            let end = foldl step (Path {north=0, east=0, facing=N}) is
            let dist = (abs $ north end) + (abs $ east end )
            print $ show dist

streetGridB :: IO ()
streetGridB = do
    raw <- readFile "data/StreetGridA.txt"
    case parse doParse "" raw  of
        Left e -> error $ show e
        Right is -> do
            let end = stepB (PathB {northB=0, eastB=0, facingB=N, visited=[(0,0)]}) is
            let dist = (abs $ northB end) + (abs $ eastB end)
            print $ show dist

stepB :: PathB -> [Instruction] -> PathB
stepB p [] = p
stepB p ((L n):rest) = let 
    (b, x) = case facingB p of
            N -> (False, p {facingB= W, eastB= (eastB p) - n})
            E -> (True, p {facingB= N, northB= (northB p) + n})
            S -> (False, p {facingB= E, eastB= (eastB p) + n})
            W -> (True, p {facingB= S, northB= (northB p) - n})
    (crossed, p') = computeVisited p x (northB p /= northB x)
    x' = x {visited = visited p'}
    in if crossed then p' else stepB x' rest
stepB p ((R n):rest) = let
    x = case facingB p of
            N -> p {facingB= E, eastB= (eastB p) + n}
            E -> p {facingB= S, northB= (northB p) - n}
            S -> p {facingB= W, eastB= (eastB p) - n}
            W -> p {facingB= N, northB= (northB p) + n}
    (crossed, p') = computeVisited p x (northB p /= northB x)
    x' = x {visited = visited p'}
    in if crossed then p' else stepB x' rest

computeVisited :: PathB -> PathB -> Bool -> (Bool, PathB)
computeVisited start end vertical = let
    f = if vertical then northB else eastB
    negative = f start > f end
    x = if negative
        then [f start -1, f start -2 .. f end]
        else [f start + 1 .. f end]
    ray = if vertical
          then [(v, eastB end)| v <- x ]
          else [(northB end, v) | v <- x]
    in run start ray
    where
    run :: PathB -> [(Int, Int)] -> (Bool, PathB)
    run p [] = (False, p)
    run p@(PathB {..}) (x:rest) =
        if elem x visited
        then (True, p {northB = fst x, eastB = snd x})
        else run (p {visited = x:visited }) rest

step :: Path -> Instruction -> Path
step p@(Path {..}) (L n)  = case facing of
    N -> p {facing= W, east= east - n}
    E -> p {facing= N, north= north + n}
    S -> p {facing= E, east= east + n}
    W -> p {facing= S, north= north - n}
step p@(Path {..}) (R n) = case facing of
    N -> p {facing= E, east= east + n}
    E -> p {facing= S, north= north - n}
    S -> p {facing= W, east= east - n}
    W -> p {facing= N, north= north + n}
    
doParse :: Parser [Instruction]
doParse = parseInstruction `sepBy` comma

comma :: Parser ()
comma = skipMany $ do
    char ','

parseInstruction :: Parser Instruction
parseInstruction = do
    dir <- oneOf ['L', 'R']
    n <- many1 digit
    let dist = read n
    case dir of
        'L' -> pure $ L dist
        'R' -> pure $ R dist
