module Solutions17.Day5 where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import Data.Array.MArray
import Data.Array.IO
import Debug.Trace

type Maze = IOUArray Int Int
type Index = Int

makeMaze ::
    [Int]
    -> IO Maze
makeMaze input = do
    let maxIdx = length input - 1
    newListArray (0, maxIdx) input

-- | increments the current pointer & moves 'n' steps left or right
executeJump ::
    Index
    -> Maze
    -> StateT Int IO (Index, Maze)
executeJump idx maze = do
    currVal <- lift $ readArray maze idx
    _ <- lift $ writeArray maze idx (currVal + 1)
    pure (idx + currVal, maze)

executeJump' ::
    Index
    -> Maze
    -> StateT Int IO (Index, Maze)
executeJump' idx maze = do
    currVal <- lift $ readArray maze idx
    let negOffset = if currVal < 0 then 1 else (-1)
    let nextVal = if abs currVal >= 3 then currVal + negOffset else currVal + 1
    _ <- lift $ writeArray maze idx nextVal
    pure (idx + currVal, maze)

escapedMaze ::
    Index
    -> Maze
    -> StateT Int IO Bool
escapedMaze index maze = do
    (start, end) <- lift $ getBounds maze
    pure $ index > end || index < start

runMaze ::
    (Index -> Maze -> StateT Int IO (Index, Maze))
    -> Maze
    -> IO Int
runMaze f maze = do
    res <- execStateT (go 0 maze) 0
    pure res
    where
        go :: Index -> Maze -> StateT Int IO (Index, Maze)
        go n m = do
            done<- escapedMaze n m
            if done
            then pure (n, m)
            else do
               modify (+1)
               (n',m') <- f n m
               go n' m'

checkA :: IO Int
checkA = do
    raw <- (fmap read . lines) <$> readFile "data/2017/Day5"
    runMaze executeJump =<< makeMaze raw

checkB :: IO Int
checkB = do
    raw <- (fmap read . lines) <$> readFile "data/2017/Day5"
    runMaze executeJump' =<< makeMaze raw

