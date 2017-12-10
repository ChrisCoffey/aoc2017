module Solutions17.Day6 where

import Control.Monad.Loops (untilM_)
import Control.Monad.State
import Crypto.Hash
import Crypto.Hash.Algorithms (MD5)
import Data.Monoid ((<>))
import Data.List (partition)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

import Debug.Trace

input :: [Int]
input = [5,1,10,0,1,7,13,14,3,12,8,10,7,12,0,6]

runCycle ::
   [Int] -- input
   -> S.Set (Digest MD5)
   -> Maybe [Int]
runCycle banks seen
    | hashList banks `S.member` seen = Nothing
    | otherwise = Just $ runCycleGuts banks

runCycleGuts ::
    [Int]
    -> [Int]
runCycleGuts banks = let
        (lt, m, gt) = findMax banks
        gt' = redistribute m gt
        m' = max (m - length gt) 0
        in redistLoop m' (lt <> (0:gt'))
    where
     redistLoop m xs
        | m > length xs = redistLoop (m - length xs) $ redistribute m xs
        | otherwise = redistribute m xs

redistribute ::
    Int
    -> [Int]
    -> [Int]
redistribute 0 xs = xs
redistribute n [] = []
redistribute n (x:xs) =
    (x+1):redistribute (n - 1) xs

findMax ::
    [Int]
    -> ( [Int], Int, [Int])
findMax xs = let
    m = maximum xs
    lt = takeWhile (< m) xs
    gt = drop 1 $ dropWhile (< m) xs
    in (lt, m, gt)

hashList ::
    [Int]
    -> Digest MD5
hashList xs =
    hash . BS.pack $ show xs

checkA :: IO Int
checkA =  pure $ go input S.empty
    where
        go xs seen =
            case runCycle xs seen of
                Nothing -> 0
                Just ys -> 1 + go ys (S.insert (hashList xs) seen)

checkB :: IO Int
checkB = pure $ go input M.empty 0
    where
        go xs seen n = let
            ys = runCycleGuts xs
            seen' = M.insert (hashList xs) n seen
            in case hashList ys `M.lookup` seen of
                Just m -> (n+1) - m
                Nothing -> go ys seen' (n+1)
