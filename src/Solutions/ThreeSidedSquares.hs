module Solutions.ThreeSidedSquares (
   trianglesA,
   trianglesB
) where

import qualified Data.Text as T

trianglesA :: IO ()
trianglesA = do 
    ls <- lines <$> readFile "data/ThreeSidedSquares.txt"
    let trips = toTriple . T.tail . T.pack <$> ls
    let possibilities = length $ filter possibleTriangle trips
    print $ show possibilities

toTriple :: T.Text -> (Int, Int, Int)
toTriple t = let 
    [a,b,c] = (\i -> read (T.unpack i) :: Int) <$> T.splitOn (T.pack "|") t
    in (a,b,c)

possibleTriangle :: (Int, Int, Int) -> Bool
possibleTriangle (a,b,c)
    | a + b > c &&
      a + c > b &&
      b + c > a = True
    | otherwise = False

trianglesB :: IO ()
trianglesB = do
    ls <- lines <$> readFile "data/ThreeSidedSquares.txt"
    let trips = peel $ T.tail . T.pack <$> ls
    let possibilities = length $ filter possibleTriangle trips
    print $ show possibilities

peel :: [T.Text] -> [(Int, Int, Int)]
peel [] = []
peel (a:b:c:rest) = let
    [x,y,z] = zip3 (listIfy a) (listIfy b) (listIfy c) 
    in x:y:z: peel rest
    where 
    listIfy :: T.Text -> [Int]
    listIfy t = (\i -> read (T.unpack i) :: Int) <$> T.splitOn (T.pack "|") t 


