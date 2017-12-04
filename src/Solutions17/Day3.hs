module Solutions17.Day3 where

import Control.Monad (forM_)
import Data.Foldable (find)
import Data.Maybe (maybe)
import qualified Data.Array.MArray as A
import qualified Data.Array.IO as A

odds :: [Int]
odds = [1,3..]

layerSize ::
    Int  -- | layer number
    -> Int
layerSize 1 = 1
layerSize n = ((odds !! (n-2)) * 4) + 4

lowerRightCornerInLayer ::
    Int -- | layer number
    -> Int
lowerRightCornerInLayer n =
    sum $ layerSize <$> [1..n]

sideInLayer ::
    Int -- | layer number
    -> Int -- | side number
    -> [Int] -- | the side
sideInLayer layer sideNum = let
    southEast = lowerRightCornerInLayer layer
    size = (odds !! (layer - 2)) + 1
    southWest = southEast - size
    northWest = southWest - size
    northEast = northWest - size
    in [(-1):[northEast-size+1 .. northEast], [northEast..northWest], [northWest..southWest], [southWest.. southEast]] !! (sideNum -1)

findLayerFor ::
    Int
    -> Int
findLayerFor n =
    maybe (-1) (\x -> x) $ find (\l -> lowerRightCornerInLayer l >= n) [1..]

findSideFor ::
    Int
    -> [Int]
findSideFor n =
    case findLayerFor n of
        (-1) -> []
        layer -> let
            sides = sideInLayer layer <$> [1..4]
            in concat $ filter (elem n) sides

newtype Point = Point (Int, Int)
manhattanDistance ::
    Point
    -> Point
    -> Int
manhattanDistance (Point (x,y)) (Point (a,b)) = abs (x - a) + abs (y - b)

puzzleInput = 277678

solvePartA :: IO Int
solvePartA = do
    let layer = findLayerFor puzzleInput
        side = findSideFor puzzleInput
        sideMidpoint = (length side `div` 2) -- This works b/c 0 index list
        sideMedian = side !! sideMidpoint
        x = layer - 1
        y = abs (sideMedian - puzzleInput)
    pure $ manhattanDistance (Point (0,0)) (Point (x,y))

type Arr a = A.IOArray Int a

eucPlane ::
    Int
    -> Int
    -> IO (Arr (Arr Int))
eucPlane maxX maxY = do
    width <- A.newArray (0, maxX) (undefined :: Arr Int)
    forM_ [0..maxX] $ \i -> do
        h <- A.newArray (0, maxY) 0
        A.writeArray width i h
    pure width
    where
        makeArr end = A.newArray (0, end) undefined

origin :: Point
origin = Point (250, 250)

spiralSeq :: IO [Int]
spiralSeq = do
    plane <- eucPlane 500 500
    undefined
    where

        go :: Int -> Int -> Arr (Arr Int) -> IO ()
        go layer side arr = undefined



